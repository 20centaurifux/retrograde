(ns retrograde.store.core
  (:require [retrograde.core :as rg]))

(defn- find-sym-form
  [sym body]
  (some (fn [[head :as form]]
          (when (= sym head)
            form))
        body))

(defn- unqualified-match?
  [v x]
  (and (nil? (namespace x))
       (= (name (:name (meta v)))
          (name x))))

(defn- find-var-form
  [v body]
  (let [{vname :name} (meta v)]
    (some (fn [[head & tail]]
            (when (or (= v (resolve head))
                      (unqualified-match? v head))
              (cons (symbol vname) tail)))
          body)))

(defn- protocol-vars
  [v]
  (let [interns (ns-interns (:ns (meta v)))]
    (->> @v
         :sigs
         keys
         (map #(get interns (symbol (name %))))
         set)))

(defn- forward-form
  [v child]
  (let [{method-name :name method-ns :ns [arg-names] :arglists} (meta v)
        method-call (symbol (str method-ns) (name method-name))
        args (mapv gensym (rest arg-names))]
    `(~(symbol method-name) [~'_ ~@args]
                            (~method-call ~child ~@args))))

(defn- decorated-resource
  [protocol-var name binding body]
  (let [[child] binding
        close-fn (or (find-sym-form 'close body)
                     `(~'close [~'_] (.close ~child)))
        closed?-fn (or (find-var-form #'rg/closed? body)
                       (forward-form #'rg/closed? child))
        protocol-fns (map (fn [protocol-fn]
                            (or (find-var-form protocol-fn body)
                                (forward-form protocol-fn child)))
                          (protocol-vars protocol-var))]
    `(deftype ~name ~binding
       java.io.Closeable
       ~close-fn
       rg/Closed
       ~closed?-fn
       ~(symbol protocol-var)
       ~@protocol-fns)))

(defmacro defdecorated-writer
  "Defines a Writer decorator type.

  `binding` must contain the decorated child writer as its first field. The
  generated type implements `java.io.Closeable`, `rg/Closed`, and `rg/Writer`.
  Any method form supplied in `body` overrides the generated implementation;
  omitted methods are forwarded to the child writer.

  A custom close implementation can be supplied as `(close [_] ...)`. By
  default, closing the decorator closes the child writer."
  [name binding & body]
  (decorated-resource #'rg/Writer name binding body))

(defmacro defdecorated-reader
  "Defines a Reader decorator type.

  `binding` must contain the decorated child reader as its first field. The
  generated type implements `java.io.Closeable`, `rg/Closed`, and `rg/Reader`.
  Any method form supplied in `body` overrides the generated implementation;
  omitted methods are forwarded to the child reader.

  A custom close implementation can be supplied as `(close [_] ...)`. By
  default, closing the decorator closes the child reader."
  [name binding & body]
  (decorated-resource #'rg/Reader name binding body))