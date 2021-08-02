;; Build a binary search tree
;; The tree should support the following operations:
;; - Inserting an integer into the tree
;; - Finding if a given integer is present in the tree
;; - Deleting an integer from the tree
;; The code must have the following characteristics
;; - The tree must be immutable
;; - The tree must be thread safe (two threads can modify the tree simultaneously)
;; - All functions in the code must have a single exit point
;; - Pay attention to memory usage
;; Extra credit:
;; - The tree must be self balancing
;; - Use your tree to find the count of the words in a huge text file

(ns bst.core
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint]))

(defn height
  "Returns height of the given tree"
  ([tree] (height tree 0))
  ([tree count]
   (if (not-empty tree)
     (max (height (:left tree) (inc count))
          (height (:right tree) (inc count)))
     count)))

(defn factor
  "Returns the balance factor of root node of the given tree"
  [{:keys [left right] :as tree}]
  (when (not-empty tree)
    (- (height left) (height right))))

(defn insert-node
  "Returns a bst after inserting a new node"
  [{:keys [root] :as tree} value]
  (cond
    (nil? root) {:root value :left nil :right nil}
    (neg? (compare value root)) (update tree :left insert-node value)
    (pos? (compare value root)) (update tree :right insert-node value)
    :else tree))
(declare create)
(defn inorder-traversal
  [{:keys [root left right] :as tree}]
  (cond (empty? tree) []
        (some? tree)  (concat (inorder-traversal left)
                              [root]
                              (inorder-traversal right))))

(defn traverse-and-balance
  "Returns a balanced bst given a sorted list of node values"
  ([nodes start  end]
   (if (empty? nodes)
     {}
     (let [mid (int (/ (+ start end) 2))]
       (when (<= start end)
         {:root (nth nodes (int (/ (+ start end) 2)))
          :left (traverse-and-balance nodes start (- mid 1))
          :right (traverse-and-balance nodes (+ mid 1) end)})))))

(defn balance
  "Returns a balanced bst given an imabalanced bst"
  [tree]
  (cond
    (empty? tree) {}
    (= 1 (count tree)) tree
    :else
    (let [nodes (inorder-traversal tree)]
      (traverse-and-balance nodes 0 (- (count nodes) 1)))))

(def insert-and-balance #(reduce (comp balance insert-node) {} %))

(defn create
  "Returns a bst when given a list of node values. If is-at-once is true then insert node and immediately 
  balance the bst else insert all nodes and then balance bst"
  [values]
  (insert-and-balance values))

(defn has?
  "Returns true if if a given integer is present in the tree else false"
  [{:keys [root left right]} value]
  (cond
    (nil? root) false
    (= value root) true
    (neg? (compare value root)) (has? left value)
    :else (has? right value)))

(defn min-node
  "Find the minimun value in a given tree"
  [{:keys [root left] :as tree}]
  (cond
    (nil? tree) nil
    (nil? left) root
    :else (min-node left)))

(defn remove-node
  "Returns a tree after removing the given node from the given tree"
  [{:keys [root left right] :as tree} value]
  (cond
    (empty? tree) {}
    (nil? tree) nil
    (neg? (compare value root)) (update tree :left remove-node value)
    (pos? (compare value root)) (update tree :right remove-node value)
    (nil? left) right
    (nil? right) left
    :else (let [min (min-node right)]
            (-> (update tree :right remove-node min)
                (assoc :root min)))))

(def remove-and-balance #((comp balance remove-node) %1 %2))

(defn read-file
  "Returns a vector containing all the words in a file"
  [file-path]
  (let [content (string/trim
                 (slurp file-path))]
    (if (empty? content)
      []
      (string/split content #"[\s+ \. \n]"))))

(defn count-nodes
  "Returns the number of nodes in a tree"
  [{:keys [left right] :as tree}]
  (if (empty? tree)
    0
    (+ 1 (count-nodes left) (count-nodes right))))

(defn count-words
  "Returns the number of unique words in a file"
  [file-path]
  (count-nodes (create (read-file file-path))))
