(ns bst.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [bst.core :as bst]))

(def test-tree {:data 2
                :left {:data 1 :left nil :right nil :height 0}
                :right {:data 7
                        :left {:data 5 :left nil :right nil :height 0}
                        :right {:data 9 :left nil :right nil :height 0}
                        :height 1}
                :height 2})

(def test-word-tree {:data "is"
                     :left {:data "and"
                            :left {:data "This" :left nil :right nil :height 0}
                            :right {:data "file"
                                    :left nil
                                    :right {:data "for" :left nil :right nil :height 0}
                                    :height 1}
                            :height 2}
                     :right {:data "text"
                             :left {:data "my"
                                    :left nil
                                    :right {:data "testing" :left nil :right nil :height 0}
                                    :height 1}
                             :right {:data "used" :left nil :right nil :height 0}
                             :height 2}
                     :height 3})

(def root-path  "./resources/test/")

(deftest create-test
  (testing "Creation of BST"
    (testing "with a list of integers"
      (is (= test-tree
             (bst/create [2 1 9 5 7]))))
    (testing "with one integer"
      (is (= {:data 2  :left nil :right nil :height 0}
             (bst/create [2]))))
    (testing "with an empty list"
      (is (empty? (bst/create []))))))

(deftest height-test
  (testing "Height of the BST"
    (testing "with a non-empty tree (more than one node)"
      (is (= 2 (bst/height test-tree))))
    (testing "with one node"
      (is (= 0 (bst/height {:data 2  :left nil :right nil :height 0}))))
    (testing "with an empty tree"
      (is (= -1 (bst/height {}))))))

(deftest factor-test
  (testing "Balance factor of BST"
    (testing "with a non-empty tree (more than one node)"
      (is (= -1 (bst/factor test-tree))))
    (testing "with one node"
      (is (= 0 (bst/factor {:data 2 :left nil :right nil :height 0}))))
    (testing "with an empty tree"
      (is (= nil (bst/factor {}))))))

(deftest has?-test
  (testing "Does BST contain the search key"
    (testing "when present in the tree"
      (is (= true (bst/has? test-tree 7))))

    (testing "when not present in the tree"
      (is (= false (bst/has? test-tree 3))))))

(deftest min-node-test
  (testing "Minimum node of a BST"
    (testing "with a non-empty tree"
      (is (= 1 (bst/min-node test-tree))))
    (testing "with one node"
      (is (= 9 (bst/min-node {:data 9 :left nil :right nil :height 0}))))
    (testing "with an empty tree"
      (is (= nil (bst/min-node {}))))))

(deftest is-right-case?-test
  (testing "Does right subtree violates the BST property"
    (testing "with a non-empty tree"
      (is (= true (bst/is-right-case? {:data 2
                                       :left nil
                                       :right {:data 4
                                               :left {:data 3 :left nil :right nil :height 0}
                                               :right nil
                                               :height 1}
                                       :height 2}))))
    (testing "with one node"
      (is (=  false (bst/is-right-case? {:data 2 :left nil :right nil :height 0}))))
    (testing "with an empty tree"
      (is (= false (bst/is-right-case? {}))))))

(deftest is-left-case?-test
  (testing "Does right subtree violates the BST property"
    (testing "with a non-empty tree"
      (is (= true (bst/is-left-case? {:data 5
                                      :right nil
                                      :left {:data 4
                                             :left {:data 3 :left nil :right nil :height 0}
                                             :right nil
                                             :height 1}
                                      :height 2}))))
    (testing "with one node"
      (is (= false (bst/is-left-case? {:data 9 :left nil :right nil :height 0}))))
    (testing "with an empty tree"
      (is (= false (bst/is-left-case? {}))))))

(deftest is-left-right-case?-test
  (testing "Does right subtree of left child violates the BST property"
    (testing "with a non-empty tree"
      (is (= true (bst/is-left-right-case? {:data 7
                                            :right nil
                                            :left {:data 5
                                                   :left nil
                                                   :right {:data 6 :left nil :right nil :height 0}
                                                   :height 1}
                                            :height 2}))))
    (testing "with one node"
      (is (= false (bst/is-left-right-case? {:data 9 :left nil :right nil :height 0}))))
    (testing "with an empty tree"
      (is (= false (bst/is-left-right-case? {}))))))

(deftest is-right-left-case?-test
  (testing "Does right subtree of left child violates the BST property"
    (testing "with a non-empty tree"
      (is (= true (bst/is-right-left-case? {:data 2
                                            :left nil
                                            :right {:data 5
                                                    :left {:data 4 :left nil :right nil :height 0}
                                                    :right nil
                                                    :height 1}
                                            :height 2}))))
    (testing "with one node"
      (is (= false (bst/is-right-left-case? {:data 9 :left nil :right nil :height 0}))))
    (testing "with an empty tree"
      (is (= false (bst/is-right-left-case? {}))))))

(deftest is-right-right-case?-test
  (testing "Does right subtree of left child violates the BST property"
    (testing "with a non-empty tree"
      (is (= true (bst/is-right-right-case? {:data 2
                                             :left nil
                                             :right {:data 5
                                                     :left nil
                                                     :right {:data 6 :left nil :right nil :height 0}
                                                     :height 1}
                                             :height 2}))))
    (testing "with one node"
      (is (= false (bst/is-right-right-case? {:data 9 :left nil :right nil :height 0}))))
    (testing "with an empty tree"
      (is (= false (bst/is-right-right-case? {}))))))

(deftest is-left-left-case?-test
  (testing "Does right subtree of left child violates the BST property"
    (testing "with a non-empty tree"
      (is (= true (bst/is-left-left-case? {:data 6
                                           :left {:data 5
                                                  :left {:data 4 :left nil :right nil :height 0}
                                                  :right nil
                                                  :height 1}
                                           :right nil
                                           :height 2}))))
    (testing "with one node"
      (is (= false (bst/is-left-left-case? {:data 9 :left nil :right nil :height 0}))))
    (testing "with an empty tree"
      (is (= false (bst/is-left-left-case? {}))))))

(deftest rotate-left-test
  (testing "Roates a tree to left"
    (testing "given a non-empty tree"
      (is (= {:data 6
              :left {:data 5
                     :left nil
                     :right nil
                     :height 0}
              :right {:data 7
                      :left nil
                      :right nil
                      :height 0}
              :height 1}
             (bst/rotate-left {:data 5
                               :right {:data 6
                                       :right {:data 7 :left nil :right nil :height 0}
                                       :left nil
                                       :height 1}
                               :left nil
                               :height 2}))))
    (testing "given tree with one node"
      (is (= {:data nil
              :left {:data 9 :left nil :right nil :height 0}
              :right nil
              :height 1}
             (bst/rotate-left {:data 9 :left nil :right nil :height 0}))))
    (testing "given an empty tree"
      (is (= {} (bst/rotate-left {}))))))

(deftest rotate-right-test
  (testing "Rotates a tree to right"
    (testing "given a non-empty tree"
      (is (= {:data 5
              :left {:data 4  :left nil :right nil :height 0}
              :right {:data 6 :left nil :right nil :height 0}
              :height 1}
             (bst/rotate-right {:data 6
                                :left {:data 5
                                       :left {:data 4 :left nil :right nil :height 0}
                                       :right nil
                                       :height 1}
                                :right nil
                                :height 2}))))
    (testing "given a tree with one node"
      (is (=  {:data nil
               :left nil
               :right {:data 9 :left nil :right nil :height 0}
               :height 1}
              (bst/rotate-right {:data 9 :left nil :right nil :height 0}))))
    (testing "given an empty tree"
      (is (= {} (bst/rotate-right {}))))))

(deftest balance-subtree-test
  (testing "Balance a tree which violates BST properties"
    (testing "with a non-empty imbalanced tree"
      (is (=  {:data 5
               :left {:data 2
                      :left {:data 1 :left nil :right nil :height 0}
                      :right nil
                      :height 1}
               :right {:data 7
                       :left nil
                       :right {:data 9 :left nil :right nil :height 0}
                       :height 1}
               :height 2}
              (bst/balance-subtree {:data 2
                                    :left {:data 1 :left nil :right nil :height 0}
                                    :right {:data 5
                                            :left nil
                                            :right {:data 7
                                                    :left nil
                                                    :right {:data 9 :left nil :right nil :height 0}
                                                    :height 1}
                                            :height 2}
                                    :height 3})))))
  (testing "with a tree with one node"
    (is (= {:data 2 :left nil :right nil :height 0}
           (bst/balance-subtree {:data 2 :left nil :right nil :height 0}))))
  (testing "with an empty tree"
    (is (= {} (bst/balance-subtree {})))))

(deftest insert-and-balance-test
  (testing "Insertion of a new node to a BST"
    (testing "with a non-empty tree"
      (is (= {:data 5
              :left {:data 2
                     :left {:data 1  :left nil :right nil :height 0}
                     :right {:data 3 :left nil :right nil :height 0}
                     :height 1}
              :right {:data 7
                      :left nil
                      :right {:data 9 :left nil :right nil :height 0}
                      :height 1}
              :height 2}
             (bst/insert-and-balance test-tree 3)))
      (testing "with  one node"
        (is (= {:data 2
                :left nil
                :right {:data 3 :left nil :right nil :height 0}
                :height 1}
               (bst/insert-and-balance  {:data 2 :left nil :right nil :height 0} 3))))
      (testing "with an empty tree"
        (is (= {:data 2 :left nil :right nil :height 0}
               (bst/insert-and-balance {} 2))))
      (testing "with a non-empty tree and duplicate entry"
        (is (= {:data 2 :left nil :right nil :height 0}
               (bst/insert-and-balance {:data 2 :left nil :right nil :height 0} 2)))))))

(deftest remove-node-test
  (testing "Removal of a node from the BST"
    (testing "with a non-empty tree"
      (is (= {:data 2
              :left {:data 1 :left nil :right nil :height 0}
              :right {:data 7
                      :left {:data 5 :left nil :right nil :height 0}
                      :right nil
                      :height 1}
              :height 2}
             (bst/remove-node  test-tree 9)))
      (testing "with one node"
        (is (= nil
               (bst/remove-node  {:data 2 :left nil :right nil :height 0} 2))))
      (testing "with an empty tree"
        (is (= {}
               (bst/remove-node {} 2)))))))

(deftest read-file-test
  (testing "Read the content of a file and returns the vector after spliting the content at whitespace"
    (testing "with a non-empty file"
      (is (= ["This" "is" "my" "text" "file"]
             (bst/read-file
              (str root-path "text.txt")))))
    (testing "with an empty file"
      (is (= []
             (bst/read-file
              (str root-path "text2.txt")))))))

(deftest count-nodes-test
  (testing "Counts the number of nodes in a tree"
    (testing "with a non-empty tree"
      (is (= 9
             (bst/count-nodes test-word-tree))))
    (testing "with a tree having one node"
      (is (= 1
             (bst/count-nodes {:data 2 :left nil :right nil :height 0}))))
    (testing "with an empty tree"
      (is (= 0
             (bst/count-nodes {}))))))

(deftest count-words-test
  (testing "Counts the number of unique words in a file"

    (testing "with a non-empty file without duplicate words"
      (is (= 5
             (bst/count-words
              (str root-path "text.txt")))))
    (testing "with a non-empty file with duplicate words"
      (is (= 9
             (bst/count-words
              (str root-path "text1.txt")))))
    (testing "with an empty file"
      (is (= 0
             (bst/count-words
              (str root-path "text2.txt")))))))
