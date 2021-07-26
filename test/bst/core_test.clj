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
  (let [tree {:root 2 
              :left {:root 1, :left nil, :right nil}
              :right 
              {:root 7
                :left {:root 5, :left nil, :right nil}
                :right {:root 9, :left nil, :right nil}}}
        expected-height 3]
    (testing "height failed"
      (is (= expected-height (height tree))))))

(deftest factor-test
  (let [tree {:root 2
              :left {:root 1, :left nil, :right nil}
              :right
              {:root 7 
               :left {:root 5, :left nil, :right nil}
               :right {:root 9, :left nil, :right nil}}}
        expected-factor -1]
    (testing "height failed"
      (is (= expected-factor (factor tree))))))

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
  (let [tree {:root 2
              :left {:root 1, :left nil, :right nil}
              :right
              {:root 7
               :left {:root 5, :left nil, :right nil}
               :right {:root 9, :left nil, :right nil}}}]
    (testing "min-node failed"
      (is (= 1 (min-node tree))))))

(deftest insert-node-test
  (let [tree {:root 2
              :left {:root 1, :left nil, :right nil}
              :right
              {:root 7
               :left {:root 5, :left nil, :right nil}
               :right {:root 9, :left nil, :right nil}}}
        expected-tree {:root 5, 
                       :left {:root 2, 
                              :left {:root 1,  :left nil :right nil}, 
                              :right {:root 3, :left nil, :right nil}}, 
                       :right {:root 7, 
                               :left nil, 
                               :right {:root 9, :left nil, :right nil}}}]
    (testing "insert-node failed"
      (is (= expected-tree (insert-node tree 3))))))

