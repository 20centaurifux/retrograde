(ns retrograde.serialized
  (:require [retrograde.core :as rg]
            [smuk.core :as smuk]))

;;; Writer

(smuk/defsmuk LockedWriter
  [child semaphore closed?]
  rg/Writer
  rg/Closed
  java.io.Closeable
  (close [_]
         (when (.compareAndSet closed? false true)
           (try
             (.close child)
             (finally
               (.release semaphore))))))

;;; Store

(defn- acquire-write-lock!
  [semaphore timeout-ms]
  (if (some? timeout-ms)
    (when-not (.tryAcquire semaphore
                           timeout-ms
                           java.util.concurrent.TimeUnit/MILLISECONDS)
      (throw (ex-info "Timed out waiting for write lock"
                      {:timeout-ms timeout-ms})))
    (.acquire semaphore)))

(deftype LockedStore [child semaphore timeout-ms]
  rg/Store
  (init [_]
    (rg/init child))

  (open-write [_]
    (acquire-write-lock! semaphore timeout-ms)

    (try
      (->LockedWriter (rg/open-write child)
                      semaphore
                      (java.util.concurrent.atomic.AtomicBoolean. false))
      (catch Throwable e
        (.release semaphore)
        (throw e))))

  (open-read [_]
    (rg/open-read child)))

(defn serialize-writes
  "Returns a store wrapper that serializes write transactions.

  Calls to `open-write` acquire an exclusive permit before opening the child
  store's writer, so at most one writer returned by this wrapper can be open at
  a time. The permit is released when that writer is closed. Read transactions
  are passed through to the child store unchanged.

  Options:
  - `:timeout-ms` - maximum time to wait for the write permit, in milliseconds.
                    Must be a positive integer. When omitted, `open-write`
                    waits until the permit becomes available."
  ([store & {:keys [timeout-ms]}]
   {:pre [(rg/store? store)]}
   (when (and (some? timeout-ms)
              (not (pos-int? timeout-ms)))
     (throw (ex-info "Invalid write lock timeout"
                     {:timeout-ms timeout-ms})))

   (->LockedStore store
                  (java.util.concurrent.Semaphore. 1)
                  timeout-ms)))