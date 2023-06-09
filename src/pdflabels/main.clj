(ns pdflabels.main
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def *lorem-ipsum* "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(defn get-instrumenter []
  (with-open [reader (io/reader "instrumenter.csv")]
    (let [[feltnavn & instr] (csv/read-csv reader)
          feltnavn (map (comp keyword
                              str/lower-case)
                        feltnavn)]
      (->> (map (partial zipmap feltnavn) instr)
           (map #(let [[num name] (str/split (:tittel %) #": ")]
                   (-> %
                       (assoc :num num)
                       (assoc :navn name))))
           (sort-by :num)
           doall))))

;(get-instrumenter)

(let [pdf-writer (com.itextpdf.kernel.pdf.PdfWriter. "test.pdf")
      pdf-document (com.itextpdf.kernel.pdf.PdfDocument. pdf-writer)
      page-size com.itextpdf.kernel.geom.PageSize/A4
      document (com.itextpdf.layout.Document. pdf-document page-size)
      newpage (com.itextpdf.layout.element.AreaBreak.)
      rows-on-page 8
      cells-in-row 3
      ]
  (.setMargins document 0.0 0.0 0.0 0.0)

  (let [r (.getPageEffectiveArea document page-size)
        cell-height (fn [c]
                      (float (- (/ (.getHeight r) rows-on-page)
                                (.getValue (.getPaddingTop c))
                                (.getValue (.getPaddingBottom c))
                                1.0)))
        cell (fn [txt size]
               (let [c (.setPadding (com.itextpdf.layout.element.Cell.)
                                    5.0)
                     h (cell-height c)]
                   ;(println (type h))
                   ;(.setHeight c 105.25)
                 (-> c
                     (.setHeight h)
                     (.add (com.itextpdf.layout.element.Paragraph. txt))
                     (.add (com.itextpdf.layout.element.Paragraph. (str h)))
                     (.add (com.itextpdf.layout.element.Paragraph. (str (.getHeight r))))
                     (.add (.setFontSize (com.itextpdf.layout.element.Paragraph. *lorem-ipsum*) (+ size 1.0))))

                 c))

        label (fn [num text url]
                (let [c (.setPaddings (com.itextpdf.layout.element.Cell.)
                                     8.0 11.0 8.0 11.0)
                      h (cell-height c)
                      qr-height 50.0
                      p (fn [size txt] (.setFontSize (com.itextpdf.layout.element.Paragraph. txt) (+ size 1.0)))
                      qr (com.itextpdf.barcodes.BarcodeQRCode. url)
                      form-x-object (.createFormXObject qr com.itextpdf.kernel.colors.ColorConstants/BLACK pdf-document)
                      qr-image (-> (com.itextpdf.layout.element.Image. form-x-object)
                                   ;(.setAutoScale true)
                                   (.setWidth qr-height)
                                   (.setHeight qr-height)

                                   ;(.setHorizontalAlignment com.itextpdf.layout.properties.HorizontalAlignment/RIGHT)
                                   )]
                  (.setProperty qr-image com.itextpdf.layout.properties.Property/FLOAT
                                com.itextpdf.layout.properties.FloatPropertyValue/LEFT)
                  (-> c
                      (.setHeight h)
                      (.add qr-image)
                      (.add (p 40 num))
                      (.add (p 12 text)))

                  c))

        make-table (fn [] (let [t (com.itextpdf.layout.element.Table. cells-in-row)]
                            (.useAllAvailableWidth t)
                            (.setFixedLayout t)
                            t))]


    (loop [elements (get-instrumenter)
           table (make-table)
           cnt 0]
      
      (cond
        (or (empty? elements)
            (= cnt (* cells-in-row rows-on-page)))
        (do (.add document table)
            (when (seq elements)
              (.add document newpage)
              (recur elements
                     (make-table)
                     0)))

        (seq elements)
        (do (.addCell table (label (:num (first elements))
                                   (:navn (first elements))
                                   (str "https://drift.styreportalen.no/assets/" (:id (first elements)))))
            (recur (rest elements)
                   table
                   (inc cnt))))))
  
  (.close document))

