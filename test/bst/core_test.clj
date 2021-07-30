(ns bst.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [bst.core :as bst]))

(def test-tree {:root 5
                :left {:root 1
                       :left nil
                       :right {:root 2 :left nil :right nil}}
                :right {:root 7
                        :left nil
                        :right {:root 9 :left nil :right nil}}})

(def test-word-tree {:root "is"
                     :left {:root "and"
                            :left {:root "This" :left nil :right nil}
                            :right {:root "file"
                                    :left nil
                                    :right {:root "for" :left nil :right nil}}}
                     :right {:root "text"
                             :left {:root "my"
                                    :left nil
                                    :right {:root "testing" :left nil :right nil}}
                             :right {:root "used" :left nil :right nil}}})

(def root-path  "/Users/priyangapkini/Clojure/bst/src/bst/")

(deftest create-test
  (testing "Creation of BST"
    (testing "with a list of node values"
      (is (= test-tree
             (bst/create [2 1 9 5 7]))))
    (testing "with one integer"
      (is (= {:root 2  :left nil :right nil}
             (bst/create [2]))))
    (testing "with an empty list"
      (is (empty? (bst/create []))))))

(deftest height-test
  (testing "Height of the BST"
    (testing "with a non-empty tree (more than one node)"
      (is (= 3 (bst/height test-tree))))
    (testing "with one node"
      (is (= 1 (bst/height {:root 2  :left nil :right nil}))))
    (testing "with an empty tree"
      (is (= 0 (bst/height {}))))))

(deftest factor-test
  (testing "Balance factor of BST"
    (testing "with a non-empty tree (more than one node)"
      (is (= 0 (bst/factor test-tree))))
    (testing "with one node"
      (is (= 0 (bst/factor {:root 2 :left nil :right nil}))))
    (testing "with an empty tree"
      (is (= nil (bst/factor {}))))))

(deftest has?-test
  (testing "Does BST contain the search key"
    (testing "when present in the tree"
      (is (= true (bst/has? test-tree 7)))

      (testing "when not present in the tree"
        (is (= false (bst/has? test-tree 3)))))))

(deftest min-node-test
  (testing "Minimum node of a BST"
    (testing "with a non-empty tree"
      (is (= 1 (bst/min-node test-tree))))
    (testing "with one node"
      (is (= 9 (bst/min-node {:root 9 :left nil :right nil}))))
    (testing "with an empty tree"
      (is (= nil (bst/min-node {}))))))

(deftest insert-node-test
  (testing "Insertion of a new node to a BST"
    (testing "with a non-empty tree"
      (is (= {:root 5
              :left {:root 1
                     :left nil
                     :right {:root 2
                             :left nil
                             :right {:root 3 :left nil :right nil}}}
              :right {:root 7
                      :left nil
                      :right {:root 9 :left nil :right nil}}}
             (bst/insert-node test-tree 3)))
      (testing "with  one node"
        (is (= {:root 2
                :left nil
                :right {:root 3 :left nil :right nil}}
               (bst/insert-node  {:root 2 :left nil :right nil}
                                 3))))
      (testing "with an empty tree"
        (is (= {:root 2 :left nil :right nil}
               (bst/insert-node {} 2))))
      (testing "with a non-empty tree and duplicate entry"
        (is (= {:root 2 :left nil :right nil}
               (bst/insert-node {:root 2 :left nil :right nil}
                                2)))))))

(deftest remove-node-test
  (testing "Removal of a node from the BST"
    (testing "with a non-empty tree"
      (is (= {:root 5
              :left {:root 1
                     :left nil
                     :right {:root 2 :left nil :right nil}}
              :right {:root 7 :left nil :right nil}}
             (bst/remove-node  test-tree 9)))
      (testing "with one node"
        (is (= nil
               (bst/remove-node  {:root 2 :left nil :right nil} 2))))
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
             (bst/count-nodes {:root 2 :left nil :right nil}))))
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

(deftest inorder-traversal-test)

(deftest traverse-andbalance-test)
(deftest balance-test)
