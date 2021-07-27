(ns bst.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [bst.core :as bst]))

(deftest create-bst-test
  (testing "Creation of BST"
    (testing "with a list of integers"
      (is (= {:root 2
              :left {:root 1 :left nil :right nil}
              :right
              {:root 7
               :left {:root 5 :left nil :right nil}
               :right {:root 9 :left nil :right nil}}}
             (bst/create-bst [2 1 9 5 7]))))
    (testing "with one integer"
      (is (= {:root 2  :left nil :right nil}
             (bst/create-bst [2]))))
    (testing "with an empty list"
      (is (empty? (bst/create-bst []))))))

(deftest height-test
  (testing "Height of the BST"
    (testing "with a non-empty tree (more than one node)"
      (is (= 3 (bst/height {:root 2
                            :left {:root 1, :left nil, :right nil}
                            :right
                            {:root 7
                             :left {:root 5, :left nil, :right nil}
                             :right {:root 9, :left nil, :right nil}}}))))
    (testing "with one node"
      (is (= 1 (bst/height {:root 2  :left nil :right nil}))))
    (testing "with an empty tree"
      (is (= 0 (bst/height {}))))))

(deftest factor-test
  (testing "Balance factor of BST"
    (testing "with a non-empty tree (more than one node)"
      (is (= -1 (bst/factor {:root 2
                             :left {:root 1 :left nil :right nil}
                             :right
                             {:root 7
                              :left {:root 5 :left nil :right nil}
                              :right {:root 9 :left nil :right nil}}}))))
    (testing "with one node"
      (is (= 0 (bst/factor {:root 2 :left nil :right nil}))))
    (testing "with an empty tree"
      (is (= nil (bst/factor {}))))))

(deftest has?-test
  (testing "Has a BST contain an integer"
    (testing "with one present in the tree"
      (is (= true (bst/has? {:root 2
                             :left {:root 1 :left nil :right nil}
                             :right
                             {:root 7
                              :left {:root 5 :left nil :right nil}
                              :right {:root 9 :left nil :right nil}}}
                            7)))

      (testing "with one not present in the tree"
        (is (= false (bst/has? {:root 2
                                :left {:root 1 :left nil :right nil}
                                :right
                                {:root 7
                                 :left {:root 5 :left nil :right nil}
                                 :right {:root 9 :left nil :right nil}}}
                               3)))))))

(deftest min-node-test
  (testing "Minimum node of a BST"
    (testing "with a non-empty tree"
      (is (= 1 (bst/min-node {:root 2
                              :left {:root 1, :left nil, :right nil}
                              :right
                              {:root 7
                               :left {:root 5, :left nil, :right nil}
                               :right {:root 9, :left nil, :right nil}}}))))
    (testing "with one node"
      (is (= 9 (bst/min-node {:root 9 :left nil :right nil}))))
    (testing "with an empty tree"
      (is (= nil (bst/min-node {}))))))

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
             (bst/insert-node {:root 2
                               :left {:root 1, :left nil, :right nil}
                               :right
                               {:root 7
                                :left {:root 5, :left nil, :right nil}
                                :right {:root 9, :left nil, :right nil}}}
                              3)))
      (testing "with  one node"
        (is (= {:root 2
                :left nil
                :right {:root 3 :left nil :right nil}}
               (bst/insert-node  {:root 2 :left nil :right nil}
                                 3))))
      (testing "with an empty tree"
        (is (= {:root 2 :left nil :right nil} (bst/insert-node {} 2)))))))

(deftest remove-node-test
  (testing "Removal  of a node from the BST"
    (testing "with a non-empty tree"
      (is (= {:root 5
              :left {:root 2
                     :left {:root 1,  :left nil :right nil}
                     :right nil}
              :right {:root 7
                      :left nil
                      :right {:root 9, :left nil, :right nil}}}
             (bst/remove-node  {:root 5
                                :left {:root 2
                                       :left {:root 1,  :left nil :right nil}
                                       :right {:root 3, :left nil, :right nil}}
                                :right {:root 7
                                        :left nil
                                        :right {:root 9, :left nil, :right nil}}}
                               3)))
      (testing "with one node"
        (is (= nil (bst/remove-node  {:root 2 :left nil :right nil}
                                  2))))
      (testing "with an empty tree"
        (is (= nil (bst/remove-node {} 2)))))))
