;;; llm-provider-utils-test.el --- Tests for llm-provider-utils -*- lexical-binding: t; package-lint-main-file: "llm.el"; -*-

;; Copyright (c) 2023-2026  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file provides functions to help build providers. It should only be used
;; by modules implementing an LLM provider.

;;; Code:

(require 'cl-macs)
(require 'llm-provider-utils)
(require 'llm)

(ert-deftest llm-provider-utils-openai-arguments ()
  (let* ((args
          (list
           ;; A required string arg
           '(:name "location"
                   :type string
                   :description "The city and state, e.g. San Francisco, CA")
           ;; A string arg with an name
           '(:name "unit"
                   :type string
                   :description "The unit of temperature, either 'celsius' or 'fahrenheit'"
                   :enum ["celsius" "fahrenheit"]
                   :optional t)
           '(:name "postal_codes"
                   :type array
                   :description "Specific postal codes"
                   :items (:type string)
                   :optional t)))
         (result (llm-provider-utils-openai-arguments args))
         (expected
          '(:type "object"
                  :properties
                  (:location
                   (:type "string"
                          :description "The city and state, e.g. San Francisco, CA")
                   :unit
                   (:type "string"
                          :description "The unit of temperature, either 'celsius' or 'fahrenheit'"
                          :enum ["celsius" "fahrenheit"])
                   :postal_codes (:type "array"
                                        :description "Specific postal codes"
                                        :items (:type "string")))
                  :required ["location"])))
    (should (equal result expected))))

(ert-deftest llm-provider-utils-parse-openai-tool-arguments ()
  (should (equal (llm-provider-utils-parse-openai-tool-arguments "")
                 nil))
  (should (equal (llm-provider-utils-parse-openai-tool-arguments
                  "{\"content\":\"├── research_plan.md\"}")
                 '((content . "├── research_plan.md"))))
  (should-error
   (llm-provider-utils-parse-openai-tool-arguments
    "{\"content\":\"├── research_plan.md\"")
   :type 'llm-tool-call-error))

(ert-deftest llm-provider-utils-openai-collect-streaming-tool-uses-invalid-json ()
  (should-error
   (llm-provider-utils-openai-collect-streaming-tool-uses
    [((index . 0)
      (id . "call_1")
      (function
       (name . "write_file")
       (arguments . "{\"content\":\"├── research_plan.md\"")))])
   :type 'llm-tool-call-error))

(ert-deftest llm-provider-utils-convert-to-serializable ()
  (should (equal (llm-provider-utils-convert-to-serializable '(:a 1 :b 2))
                 '(:a 1 :b 2)))
  (should (equal (llm-provider-utils-convert-to-serializable '(:a "1" :b foo))
                 '(:a "1" :b "foo")))
  (should (equal (llm-provider-utils-convert-to-serializable '(:inner '(:a foo :b bar)))
                 '(:inner '(:a "foo" :b "bar")))))

(ert-deftest llm-provider-utils-append-to-prompt ()
  (let ((prompt (llm-make-chat-prompt "Prompt")))
    (llm-provider-utils-append-to-prompt prompt '(:a 1 :b :json-false)
                                         (list
                                          (make-llm-chat-prompt-tool-result
                                           :tool-name "tool"
                                           :result :json-false)))
    (should (equal (nth 1 (llm-chat-prompt-interactions prompt))
                   (make-llm-chat-prompt-interaction
                    :role 'tool-results
                    :content "(:a 1 :b nil)"
                    :tool-results (list
                                   (make-llm-chat-prompt-tool-result
                                    :tool-name "tool"
                                    :result :false)))))))

(ert-deftest llm-provider-utils-combine-to-system-prompt ()
  (let* ((interaction1 (make-llm-chat-prompt-interaction :role 'user :content "Hello"))
         (example1 (cons "Request 1" "Response 1"))
         (example2 (cons "Request 2" "Response 2"))
         (prompt-for-first-request
          (make-llm-chat-prompt
           :context "Example context"
           :interactions (list (copy-llm-chat-prompt-interaction interaction1))
           :examples (list example1 example2)))
         (prompt-with-existing-system-prompt
          (make-llm-chat-prompt
           :context "Example context"
           :interactions (list
                          (make-llm-chat-prompt-interaction :role 'system :content "Existing system prompt.")
                          (copy-llm-chat-prompt-interaction interaction1))
           :examples (list example1 example2))))
    (llm-provider-utils-combine-to-system-prompt prompt-for-first-request)
    (should (= 2 (length (llm-chat-prompt-interactions prompt-for-first-request))))
    (should (equal "Example context\nHere are 2 examples of how to respond:\n\nUser: Request 1\nAssistant: Response 1\nUser: Request 2\nAssistant: Response 2"
                   (llm-chat-prompt-interaction-content (nth 0 (llm-chat-prompt-interactions prompt-for-first-request)))))
    (should (equal "Hello" (llm-chat-prompt-interaction-content (nth 1 (llm-chat-prompt-interactions prompt-for-first-request)))))
    (should-not (llm-chat-prompt-context prompt-for-first-request))
    (should-not (llm-chat-prompt-examples prompt-for-first-request))

    ;; On the request with the existing system prompt, it should append the new
    ;; text to the existing system prompt.
    (llm-provider-utils-combine-to-system-prompt prompt-with-existing-system-prompt)
    (should (= 2 (length (llm-chat-prompt-interactions prompt-with-existing-system-prompt))))
    (should (equal "Existing system prompt.\nExample context\nHere are 2 examples of how to respond:\n\nUser: Request 1\nAssistant: Response 1\nUser: Request 2\nAssistant: Response 2"
                   (llm-chat-prompt-interaction-content (nth 0 (llm-chat-prompt-interactions prompt-with-existing-system-prompt)))))))

(ert-deftest llm-provider-utils-combine-to-user-prompt ()
  (let* ((interaction1 (make-llm-chat-prompt-interaction :role 'user :content "Hello"))
         (example1 (cons "Request 1" "Response 1"))
         (example2 (cons "Request 2" "Response 2"))
         (prompt-for-first-request
          (make-llm-chat-prompt
           :context "Example context"
           :interactions (list (copy-llm-chat-prompt-interaction interaction1))
           :examples (list example1 example2))))
    ;; In the first request, the system prompt should be prepended to the user request.
    (llm-provider-utils-combine-to-user-prompt prompt-for-first-request)
    (should (= 1 (length (llm-chat-prompt-interactions prompt-for-first-request))))
    (should-not (llm-chat-prompt-context prompt-for-first-request))
    (should-not (llm-chat-prompt-examples prompt-for-first-request))
    (should (equal "Example context\nHere are 2 examples of how to respond:\n\nUser: Request 1\nAssistant: Response 1\nUser: Request 2\nAssistant: Response 2\nHello"
                   (llm-chat-prompt-interaction-content (nth 0 (llm-chat-prompt-interactions prompt-for-first-request)))))))

(ert-deftest llm-provider-utils-collapse-history ()
  (let* ((interaction1 (make-llm-chat-prompt-interaction :role 'user :content "Hello"))
         (interaction2 (make-llm-chat-prompt-interaction :role 'assistant :content "Hi! How can I assist you?"))
         (interaction3 (make-llm-chat-prompt-interaction :role 'assistant :content "Earl Grey, hot."))
         (prompt-for-first-request
          (make-llm-chat-prompt
           :interactions (list (copy-llm-chat-prompt-interaction interaction1))))
         (prompt-for-second-request
          (make-llm-chat-prompt
           :interactions (list (copy-llm-chat-prompt-interaction interaction1)
                               (copy-llm-chat-prompt-interaction interaction2)
                               (copy-llm-chat-prompt-interaction interaction3)))))
    ;; In the first request, there's no history, so nothing should be done.
    (llm-provider-utils-collapse-history prompt-for-first-request)
    (should (= 1 (length (llm-chat-prompt-interactions prompt-for-first-request))))
    (should (equal interaction1 (nth 0 (llm-chat-prompt-interactions prompt-for-first-request))))

    ;; In the second request we should have the history prepended.
    (llm-provider-utils-collapse-history prompt-for-second-request)
    (should (= 1 (length (llm-chat-prompt-interactions prompt-for-first-request))))
    (should (equal "Previous interactions:\n\nUser: Hello\nAssistant: Hi! How can I assist you?\n\nThe current conversation follows:\n\nEarl Grey, hot."
                   (llm-chat-prompt-interaction-content (nth 0 (llm-chat-prompt-interactions prompt-for-second-request)))))))

(ert-deftest llm-provider-utils-streaming-accumulate ()
  (should (equal 3 (llm-provider-utils-streaming-accumulate 1 2)))
  (should (equal "foobar" (llm-provider-utils-streaming-accumulate "foo" "bar")))
  (should (equal [1 2 3] (llm-provider-utils-streaming-accumulate [1] [2 3])))
  (should (equal '(1 2 3) (llm-provider-utils-streaming-accumulate '(1) '(2 3))))
  (should (equal (llm-test-normalize '(:foo "aa" :bar "b" :baz "c"))
                 (llm-test-normalize (llm-provider-utils-streaming-accumulate '(:foo "a" :baz "c") '(:foo "a" :bar "b")))))
  (should (equal '(:foo 3) (llm-provider-utils-streaming-accumulate '(:foo 1) '(:foo 2))))
  (should (equal '(:foo "foo bar baz") (llm-provider-utils-streaming-accumulate '(:foo "foo bar") '(:foo " baz")))))

(ert-deftest llm-provider-utils--normalize-args ()
  (should-not (llm-provider-utils--normalize-args :false))
  (should-not (llm-provider-utils--normalize-args :json-false))
  (should (equal '(1 2 nil)
                 (llm-provider-utils--normalize-args '(1 2 :json-false))))
  (should (equal [1 2 nil]
                 (llm-provider-utils--normalize-args [1 2 :json-false])))
  (should (equal '(1 2 [t nil t])
                 (llm-provider-utils--normalize-args '(1 2 [t :false t]))))
  (should (equal '(:a 1 :b nil)
                 (llm-provider-utils--normalize-args '(:a 1 :b :json-false))))
  (should (equal '((a . 1) (b . nil))
                 (llm-provider-utils--normalize-args '((a . 1) (b . :json-false))))))

(cl-defstruct llm-testing-provider (llm-standard-chat-provider) ())

(cl-defmethod llm-provider-populate-tool-uses ((provider llm-testing-provider)
                                               prompt tool-uses))

(cl-defmethod llm-provider-append-to-prompt ((provider llm-testing-provider)
                                             prompt content
                                             &optional tool-results)
  (llm-provider-utils-append-to-prompt prompt content tool-results
                                       (if tool-results
                                           'user
                                         'assistant)))

(ert-deftest llm-provider-utils-execute-tool-uses--parallel-same-tool ()
  (let ((prompt (llm-make-chat-prompt
                 ""
                 :tools (list (llm-make-tool :name "a"
                                             :function (lambda (callback arg)
                                                         (funcall callback (format "Result for %s" arg)))
                                             :args '((:name "argument"
                                                      :type integer
                                                      :description "An argument"))
                                             :async t)))))
    (llm-provider-utils-execute-tool-uses
     (make-llm-testing-provider)
     prompt
     (list
      (make-llm-provider-utils-tool-use
       :id "1"
       :name "a"
       :args '((argument . "foo")))
      (make-llm-provider-utils-tool-use
       :id "2"
       :name "a"
       :args '((argument . "bar"))))
     t
     nil
     #'ignore)
    (let* ((last-interaction (car (last (llm-chat-prompt-interactions prompt))))
           (tool-results (llm-chat-prompt-interaction-tool-results last-interaction)))
      (dolist (id '("1" "2"))
        (should (seq-find (lambda (result) (equal (llm-chat-prompt-tool-result-call-id result)
                                                  id))
                          tool-results))))))

(ert-deftest llm-provider-utils-execute-tool-uses--no-args ()
  (let* ((prompt (llm-make-chat-prompt
                  ""
                  :tools (list (llm-make-tool
                                :name "a"
                                :function (lambda () 'success)))))
         result)
    (llm-provider-utils-execute-tool-uses
     (make-llm-testing-provider)
     prompt
     (list
      (make-llm-provider-utils-tool-use
       :id "1"
       :name "a"
       :args nil))
     t
     nil
     (lambda (r) (setq result r)))
    (should (equal result
                   '(:tool-results
                     ((:id "1" :name "a" :status success :result success)))))))

(ert-deftest llm-provider-utils-execute-tool-uses--missing-tool ()
  (let (result)
    (llm-provider-utils-execute-tool-uses
     (make-llm-testing-provider)
     (llm-make-chat-prompt
      ""
      :tools (list
              (llm-make-tool
               :name "tool-a"
               :description "Tool A"
               :function (lambda (&rest args) "Result A")
               :args '())))
     (list
      (make-llm-provider-utils-tool-use
       :id "1"
       :name "tool-b"
       :args '()))
     nil
     nil
     (lambda (value) (setq result value)))
    (should (equal (plist-get (car result) :id) "1"))
    (should (eq (plist-get (car result) :status) 'error))
    (should (eq (plist-get (plist-get (car result) :error) :type)
                'llm-tool-unknown-tool))))

(ert-deftest llm-provider-utils-execute-tool-uses--unknown-arg ()
  (let (result)
    (llm-provider-utils-execute-tool-uses
     (make-llm-testing-provider)
     (llm-make-chat-prompt
      ""
      :tools (list
              (llm-make-tool
               :name "tool-a"
               :description "Tool A"
               :function (lambda (&rest args) "Result A")
               :args '((:name "arg1" :type string :description "Argument 1")))))
     (list
      (make-llm-provider-utils-tool-use
       :id "1"
       :name "tool-a"
       :args '((arg1 . "value1")
               (arg2 . "value2"))))
     nil
     nil
     (lambda (value) (setq result value)))
    (should (eq (plist-get (car result) :status) 'error))
    (should (eq (plist-get (plist-get (car result) :error) :type)
                'llm-tool-unknown-argument))))

(ert-deftest llm-provider-utils-execute-tool-uses--missing-arg ()
  (let (result)
    (llm-provider-utils-execute-tool-uses
     (make-llm-testing-provider)
     (llm-make-chat-prompt
      ""
      :tools (list
              (llm-make-tool
               :name "tool-a"
               :description "Tool A"
               :function (lambda (&rest args) "Result A")
               :args '((:name "arg1" :type string :description "Argument 1")))))
     (list
      (make-llm-provider-utils-tool-use
       :id "1"
       :name "tool-a"
       :args '()))
     nil
     nil
     (lambda (value) (setq result value)))
    (should (eq (plist-get (car result) :status) 'error))
    (should (eq (plist-get (plist-get (car result) :error) :type)
                'llm-tool-missing-argument))))

(ert-deftest llm-provider-utils-execute-tool-uses--missing-optional-arg ()
  (llm-provider-utils-execute-tool-uses
   (make-llm-testing-provider)
   (llm-make-chat-prompt
    ""
    :tools (list
            (llm-make-tool
             :name "tool-a"
             :description "Tool A"
             :function (lambda (&rest args) "Result A")
             :args '((:name "arg1" :type string :description "Argument 1" :optional t)))))
   (list
    (make-llm-provider-utils-tool-use
     :id "1"
     :name "tool-a"
     :args '()))
   nil
   nil
   #'ignore))


(ert-deftest llm-provider-utils-execute-tool-uses--mixed-results ()
  (let* ((prompt
          (llm-make-chat-prompt
           ""
           :tools
           (list
            (llm-make-tool :name "ok" :function (lambda () 42) :args nil)
            (llm-make-tool :name "bad"
                           :function (lambda () (error "Tool failed"))
                           :args nil))))
         result)
    (llm-provider-utils-execute-tool-uses
     (make-llm-testing-provider)
     prompt
     (list
      (make-llm-provider-utils-tool-use :id "call-ok" :name "ok" :args nil)
      (make-llm-provider-utils-tool-use :id "call-bad" :name "bad" :args nil))
     nil
     nil
     (lambda (value) (setq result value)))
    (should (equal (mapcar (lambda (outcome) (plist-get outcome :id)) result)
                   '("call-ok" "call-bad")))
    (should (eq (plist-get (nth 0 result) :status) 'success))
    (should (= (plist-get (nth 0 result) :result) 42))
    (should (eq (plist-get (nth 1 result) :status) 'error))
    (should (eq (plist-get (plist-get (nth 1 result) :error) :type) 'error))
    (should (string-match-p
             "Tool failed"
             (plist-get (plist-get (nth 1 result) :error) :message)))
    (let* ((interaction (car (last (llm-chat-prompt-interactions prompt))))
           (prompt-results (llm-chat-prompt-interaction-tool-results interaction)))
      (should (equal (mapcar #'llm-chat-prompt-tool-result-call-id prompt-results)
                     '("call-ok" "call-bad")))
      (should (= (llm-chat-prompt-tool-result-result (nth 0 prompt-results)) 42))
      (should (string-match-p
               "Tool call failed (error): Tool failed"
               (llm-chat-prompt-tool-result-result (nth 1 prompt-results)))))))

(ert-deftest llm-provider-utils-execute-tool-uses--async-out-of-order ()
  (let* (callbacks
         result
         (prompt
          (llm-make-chat-prompt
           ""
           :tools
           (list
            (llm-make-tool
             :name "async-tool"
             :function (lambda (callback argument)
                         (push (cons argument callback) callbacks))
             :args '((:name "argument" :type string :description "An argument"))
             :async t)))))
    (llm-provider-utils-execute-tool-uses
     (make-llm-testing-provider)
     prompt
     (list
      (make-llm-provider-utils-tool-use
       :id "first" :name "async-tool" :args '((argument . "first")))
      (make-llm-provider-utils-tool-use
       :id "second" :name "async-tool" :args '((argument . "second"))))
     nil
     nil
     (lambda (value) (setq result value)))
    (funcall (cdr (assoc "second" callbacks)) nil '(error "Async failure"))
    (should-not result)
    (funcall (cdr (assoc "first" callbacks)) "First result")
    (should (equal (mapcar (lambda (outcome) (plist-get outcome :id)) result)
                   '("first" "second")))
    (should (eq (plist-get (nth 0 result) :status) 'success))
    (should (equal (plist-get (nth 0 result) :result) "First result"))
    (should (eq (plist-get (nth 1 result) :status) 'error))
    (should (equal (plist-get (plist-get (nth 1 result) :error) :message)
                   "Async failure"))))

(ert-deftest llm-provider-utils-execute-tool-uses--generated-id-and-nil-result ()
  (let* ((tool-use
          (make-llm-provider-utils-tool-use :name "nil-result" :args nil))
         (prompt
          (llm-make-chat-prompt
           ""
           :tools
           (list
            (llm-make-tool :name "nil-result" :function (lambda () nil) :args nil))))
         result)
    (llm-provider-utils-execute-tool-uses
     (make-llm-testing-provider)
     prompt
     (list tool-use)
     t
     (list :tool-uses (list tool-use))
     (lambda (value) (setq result value)))
    (let* ((tool-use-result (car (plist-get result :tool-uses)))
           (outcome (car (plist-get result :tool-results)))
           (id (plist-get outcome :id)))
      (should (string-match-p "\\`llm-tool-call-[0-9]+\\'" id))
      (should (equal (plist-get tool-use-result :id) id))
      (should (equal (llm-provider-utils-tool-use-id tool-use) id))
      (should (eq (plist-get outcome :status) 'success))
      (should (plist-member outcome :result))
      (should-not (plist-get outcome :result)))))

(provide 'llm-provider-utils-test)
;;; llm-provider-utils-test.el ends here
