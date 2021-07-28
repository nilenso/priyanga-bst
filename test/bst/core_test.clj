(ns bst.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [bst.core :as bst]))

(def test-tree {:root 2
                :left {:root 1 :left nil :right nil}
                :right {:root 7
                        :left {:root 5 :left nil :right nil}
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

(deftest create-test
  (testing "Creation of BST"
    (testing "with a list of integers"
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
      (is (= -1 (bst/factor test-tree))))
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

(deftest is-right-case?-test
  (testing "Does right subtree violates the BST property"
    (testing "with a non-empty tree"
      (is (= true (bst/is-right-case? {:root 2
                                       :left nil
                                       :right {:root 4
                                               :left {:root 3 :left nil :right nil}
                                               :right nil}}))))
    (testing "with one node"
      (is (=  false (bst/is-right-case? {:root 2 :left nil :right nil}))))
    (testing "with an empty tree"
      (is (= false (bst/is-right-case? {}))))))

(deftest is-left-case-test
  (testing "Does right subtree violates the BST property"
    (testing "with a non-empty tree"
      (is (= true (bst/is-left-case? {:root 5
                                      :right nil
                                      :left {:root 4
                                             :left {:root 3 :left nil :right nil}
                                             :right nil}}))))
    (testing "with one node"
      (is (= false (bst/is-left-case? {:root 9 :left nil :right nil}))))
    (testing "with an empty tree"
      (is (= false (bst/is-left-case? {}))))))

(deftest is-left-right-case-test
  (testing "Does right subtree of left child violates the BST property"
    (testing "with a non-empty tree"
      (is (= true (bst/is-left-right-case? {:root 7
                                            :right nil
                                            :left {:root 5
                                                   :left nil
                                                   :right {:root 6 :left nil :right nil}}}))))
    (testing "with one node"
      (is (= false (bst/is-left-right-case? {:root 9 :left nil :right nil}))))
    (testing "with an empty tree"
      (is (= false (bst/is-left-right-case? {}))))))

(deftest is-right-left-case-test
  (testing "Does right subtree of left child violates the BST property"
    (testing "with a non-empty tree"
      (is (= true (bst/is-right-left-case? {:root 2
                                            :left nil
                                            :right {:root 5
                                                    :left {:root 4 :left nil :right nil}
                                                    :right nil}}))))
    (testing "with one node"
      (is (= false (bst/is-right-left-case? {:root 9 :left nil :right nil}))))
    (testing "with an empty tree"
      (is (= false (bst/is-right-left-case? {}))))))

(deftest is-right-right-case-test
  (testing "Does right subtree of left child violates the BST property"
    (testing "with a non-empty tree"
      (is (= true (bst/is-right-right-case? {:root 2
                                             :left nil
                                             :right {:root 5
                                                     :left nil
                                                     :right {:root 6 :left nil :right nil}}}))))
    (testing "with one node"
      (is (= false (bst/is-right-right-case? {:root 9 :left nil :right nil}))))
    (testing "with an empty tree"
      (is (= false (bst/is-right-right-case? {}))))))

(deftest is-left-left-case-test
  (testing "Does right subtree of left child violates the BST property"
    (testing "with a non-empty tree"
      (is (= true (bst/is-left-left-case? {:root 6
                                           :left {:root 5
                                                  :left {:root 4 :left nil :right nil}
                                                  :right nil}
                                           :right nil}))))
    (testing "with one node"
      (is (= false (bst/is-left-left-case? {:root 9 :left nil :right nil}))))
    (testing "with an empty tree"
      (is (= false (bst/is-left-left-case? {}))))))

(deftest rotate-left-test
  (testing "Roates a tree to left"
    (testing "given a non-empty tree"
      (is (= {:root 6
              :left {:root 5
                     :left nil
                     :right {:root 7
                             :left nil
                             :right nil}}
              :right nil}
             (bst/rotate-left {:root 5
                               :right {:root 6
                                       :left {:root 7 :left nil :right nil}
                                       :right nil}
                               :left nil}))))
    (testing "given tree with one node"
      (is (= {:root nil
              :left {:root 9 :left nil :right nil}
              :right nil}
             (bst/rotate-left {:root 9 :left nil :right nil}))))
    (testing "given an empty tree"
      (is (= {} (bst/rotate-left {}))))))

(deftest rotate-right-test
  (testing "Rotates a tree to right"
    (testing "given a non-empty tree"
      (is (= {:root 5
              :left {:root 4  :left nil :right nil}
              :right {:root 6 :left nil :right nil}}
             (bst/rotate-right {:root 6
                                :left {:root 5
                                       :left {:root 4 :left nil :right nil}
                                       :right nil}
                                :right nil}))))
    (testing "given a tree with one node"
      (is (=  {:root nil, :left nil, :right {:root 9, :left nil, :right nil}} (bst/rotate-right {:root 9 :left nil :right nil}))))
    (testing "given an empty tree"
      (is (= {} (bst/rotate-right {}))))))

(deftest balance-test
  (testing "Balance a tree which violates BST properties")
  (testing "with a non-empty imbalanced tree"
    (is (=  {:root 5
             :left {:root 2
                    :left {:root 1 :left nil :right nil}
                    :right nil}
             :right {:root 7
                     :left nil
                     :right {:root 9, :left nil, :right nil}}}
            (bst/balance {:root 2
                          :left {:root 1 :left nil :right nil}
                          :right {:root 5
                                  :left nil
                                  :right {:root 7
                                          :left nil
                                          :right {:root 9 :left nil :right nil}}}}))))
  (testing "with a tree with one node"
    (is (= {:root 2 :left nil :right nil}
           (bst/balance {:root 2 :left nil :right nil}))))
  (testing "with an empty tree"
    (is (= {} (bst/balance {})))))

(deftest insert-node-test
  (testing "Insertion of a new node to a BST"
    (testing "with a non-empty tree"
      (is (= {:root 5
              :left {:root 2
                     :left {:root 1,  :left nil :right nil}
                     :right {:root 3, :left nil, :right nil}}
              :right {:root 7
                      :left nil
                      :right {:root 9, :left nil, :right nil}}}
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
      (is (= {:root 2
              :left {:root 1 :left nil :right nil}
              :right {:root 7
                      :left {:root 5 :left nil :right nil}
                      :right nil}}
             (bst/remove-node  test-tree
                               9)))
      (testing "with one node"
        (is (= nil
               (bst/remove-node  {:root 2 :left nil :right nil}
                                 2))))
      (testing "with an empty tree"
        (is (= {}
               (bst/remove-node {} 2)))))))

(deftest read-file-test
  (let [root-path  "/Users/priyangapkini/Clojure/bst/src/bst/"]
    (testing "Read the content of a file and returns the vector after spliting the content at whitespace"
      (testing "with a non-empty file"
        (is (= ["This" "is" "my" "text" "file"]
               (bst/read-file
                (str root-path "text1.txt")))))
      (testing "with an empty file"
        (is (= []
               (bst/read-file
                (str root-path "text2.txt"))))))))

(deftest count-nodes-test
  (testing "Counts the number of nodes in a tree"
    (testing "with a non-empty tree"
      (is (= 9 (bst/count-nodes test-word-tree))))
    (testing "with a tree having one node"
      (is (= 1 (bst/count-nodes {:root 2 :left nil :right nil}))))
    (testing "with an empty tree"
      (is (= 0 (bst/count-nodes {}))))))
