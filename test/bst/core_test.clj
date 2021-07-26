(ns bst.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [bst.core :refer [create-bst 
                              height 
                              factor 
                              has? 
                              min-node 
                              insert-node 
                              remove-node]]))

(deftest create-bst-test
  (let [expected-tree [{:root 2
                        :left {:root 1, :left nil, :right nil}
                        :right
                        {:root 7
                         :left {:root 5, :left nil, :right nil}
                         :right {:root 9, :left nil, :right nil}}}
                       
                       {:root 2, :left nil, :right nil}]] 
  (testing "Create-bst"
    (testing "with list of integer values"
      (is (= (expected-tree 0)(create-bst [2 1 9 5 7]))))
    (testing "with one value"
      (is (= (expected-tree 1) (create-bst [2]))))
    (testing "with empty list"
      (is (empty? (create-bst [])))))))

(deftest height-test
  (let [tree [{:root 2
               :left {:root 1, :left nil, :right nil}
               :right
               {:root 7
                :left {:root 5, :left nil, :right nil}
                :right {:root 9, :left nil, :right nil}}}
              
              {:root 2, :left nil, :right nil}
              
              {}]
        expected-height [3 1 0]]
    (testing "Height"
      (testing "with non-empty tree (more than one node)"
        (is (= (expected-height 0) (height (tree 0)))))
      (testing "with root-tree"
        (is (= (expected-height 1) (height (tree 1)))))
      (testing "with empty tree"
        (is (= (expected-height 2) (height (tree 2))))))))

(deftest factor-test
  (let [tree [{:root 2
               :left {:root 1, :left nil, :right nil}
               :right
               {:root 7
                :left {:root 5, :left nil, :right nil}
                :right {:root 9, :left nil, :right nil}}}
              
              {:root 2, :left nil, :right nil} 

              {}]
        expected-factor [-1 0 nil]]
    (testing "Factor"
      (testing "with non-empty tree (more than one node)"
        (is (= (expected-factor 0) (factor (tree 0)))))
      (testing "with root-tree"
        (is (= (expected-factor 1) (factor (tree 1)))))
      (testing "with empty tree"
        (is (= (expected-factor 2) (factor (tree 2))))))))

  (deftest has?-test
    (let [tree {:root 2
                :left {:root 1, :left nil, :right nil}
                :right
                {:root 7
                 :left {:root 5, :left nil, :right nil}
                 :right {:root 9, :left nil, :right nil}}}]
      (testing "has? failed"
        (is (= true (has? tree 7)))
        (is (= false (has? tree 3))))))

(deftest min-node-test
  (let [tree [{:root 2
               :left {:root 1, :left nil, :right nil}
               :right
               {:root 7
                :left {:root 5, :left nil, :right nil}
                :right {:root 9, :left nil, :right nil}}}
              
              {:root 9 :left nil :right nil}
              
              {}]]
    (testing "Min-node"
      (testing "with non-empty tree" 
        (is (= 1 (min-node (tree 0)))))
      (testing "with root-tree"
        (is (= 9 (min-node (tree 1)))))
      (testing "with empty tree"
        (is (= nil (min-node (tree 2))))))))

(deftest insert-node-test
  (let [tree [{:root 2
               :left {:root 1, :left nil, :right nil}
               :right
               {:root 7
                :left {:root 5, :left nil, :right nil}
                :right {:root 9, :left nil, :right nil}}}
              
              {:root 2 :left nil :right nil}
              
              {}]
        expected-tree [{:root 5
                        :left {:root 2
                               :left {:root 1,  :left nil :right nil}
                               :right {:root 3, :left nil, :right nil}}
                        :right {:root 7
                                :left nil
                                :right {:root 9, :left nil, :right nil}}}
                       
                       {:root 2
                        :left nil
                        :right {:root 3 :left nil :right nil}}
                       
                       {:root 2 :left nil :right nil}]]
    (testing "insert-node"
      (testing "with non-empty tree"
        (is (= (expected-tree 0) (insert-node (tree 0) 3))))
      (testing "with root tree"
        (is (= (expected-tree 1) (insert-node (tree 1) 3))))
      (testing "with empty tree"
        (is (= (expected-tree 2) (insert-node (tree 2) 2)))))))

