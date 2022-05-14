#lang racket

(require data-science-master)
(require net/url)
(require plot)
(require math)
(require json)
;(require racket/date)
(require srfi/19)
(require pict/pict/info)




;(define mobi(string->url "http://textfiles.com/stories/reality.txt"))

(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json (current-input-port))])
    (if (or (eof-object? record)
            (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num) (cons record json-array)
              (read-json (current-input-port))))))

;;; Normalize case, remove URLs, remove punctuation, and remove spaces
;;; from each tweet. This function takes a list of words and returns a
;;; preprocessed subset of words/tokens as a list
(define (preprocess-text lst)
  (map (λ (x)
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase x)))))
       lst))

;;; Read in the entire tweet database (3200 tweets from Trump’s timeline)
(define tweets (string->jsexpr
                (with-input-from-file "datasets/daily_monitor.json" (λ () (json-lines->json-array)))))

;;; Remove just the tweet text and source from each tweet
;;; hash. Finally, remove retweets.
;;; Remove just the tweet text, source, and timestamp from each tweet
;;; hash. Finally, remove retweets.
(define extract
  (let ([tmp (map (λ (x) (list (hash-ref x `full_text)(hash-ref x `favorite_count)
                               (hash-ref x `created_at)(hash-ref x `source))) tweets)])
    (filter (λ (x) (not (string-prefix? (first x) "R"))) tmp)))

; Joining tweets and arranging tweets to their systematic flow in and out.

(define joined-tweets
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (first(first tlist1)))))]
                   )
             )
           ](joined1 extract "")) )

;;; returns a list of pairs. Each pair consists of a unique word 

(define words (document->tokens joined-tweets #:sort? #t))

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label. 
(define sentiment (list->sentiment words #:lexicon 'nrc))

(take sentiment 5)

;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))

(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
            #:x-label "Moods"
            #:title "Sentiment Analysis in Uganda Tweets"
            #:y-label "No of Tweets")))
            









