(defpackage :pango-test
  (:use :fiveam :common-lisp)
  (:export #:run!
           #:approx-equal)
  (:import-from :glib-test #:approx-equal)
  (:import-from :pango)
  (:import-from :cairo)
  (:import-from :gobject))

(in-package :pango-test)

(defvar *first-run-pango-test* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Set the current package
  (setf (glib-sys:get-current-package) "cl-cffi-pango")
  ;; Ensure directory for the output of test results
  (ensure-directories-exist
      (asdf:system-relative-pathname :cl-cffi-pango "test/out/"))
  ;; Set the local language, it is German for me
  (cl-setlocale:setlocale :LC-ALL ""))

(defvar *sample-text-1*
  "Weit hinten, hinter den Wortbergen, fern der Länder Vokalien und Konsonantien
leben die Blindtexte. Abgeschieden wohnen Sie in Buchstabenhausen an der Küste
des Semantik, eines großen Sprachozeans. Ein kleines Bächlein namens Duden
fließt durch ihren Ort und versorgt sie mit den nötigen Regelialien. Es ist ein
paradiesmatisches Land, in dem einem gebratene Satzteile in den Mund fliegen.
Nicht einmal von der allmächtigen Interpunktion werden die Blindtexte beherrscht
– ein geradezu unorthographisches Leben.

Eines Tages aber beschloss eine kleine Zeile Blindtext, ihr Name war Lorem
Ipsum, hinaus zu gehen in die weite Grammatik. Der große Oxmox riet ihr davon
ab, da es dort wimmele von bösen Kommata, wilden Fragezeichen und hinterhältigen
Semikola, doch das Blindtextchen ließ sich nicht beirren. Es packte seine sieben
Versalien, schob sich sein Initial in den Gürtel und machte sich auf den Weg.

Als es die ersten Hügel des Kursivgebirges erklommen hatte, warf es einen
letzten Blick zurück auf die Skyline seiner Heimatstadt Buchstabenhausen, die
Headline von Alphabetdorf und die Subline seiner eigenen Straße, der
Zeilengasse. Wehmütig lief ihm eine rhetorische Frage über die Wange, dann
setzte es seinen Weg fort.

Unterwegs traf es eine Copy. Die Copy warnte das Blindtextchen, da, wo sie
herkäme, wäre sie zigmal umgeschrieben worden und alles, was von ihrem Ursprung
noch übrig wäre, sei das Wort »und« und das Blindtextchen solle umkehren und
wieder in sein eigenes, sicheres Land zurückkehren.

Doch alles Gutzureden konnte es nicht überzeugen und so dauerte es nicht lange,
bis ihm ein paar heimtückische Werbetexter auflauerten, es mit Longe und Parole
betrunken machten und es dann in ihre Agentur schleppten, wo sie es für ihre
Projekte wieder und wieder missbrauchten. Und wenn es nicht umgeschrieben wurde,
dann benutzen Sie es immer noch.")

(defvar *sample-text-2*
  "Dies ist ein Typoblindtext.
An ihm kann man sehen, ob alle Buchstaben da sind und wie sie aussehen. Manchmal
benutzt man Worte wie Hamburgefonts, Rafgenduks oder Handgloves, um Schriften zu
testen. Manchmal Sätze, die alle Buchstaben des Alphabets enthalten – man nennt
diese Sätze »Pangramme«. Sehr bekannt ist dieser: The quick brown fox jumps over
the lazy dog. Oft werden in Typoblindtexte auch fremdsprachige Satzteile
eingebaut (AVAIL® and Wefox™ are testing aussi la Kerning), um die Wirkung in
anderen Sprachen zu testen. In Lateinisch sieht zum Beispiel fast jede Schrift
gut aus. Quod erat demonstrandum.

Seit 1975 fehlen in den meisten Testtexten die Zahlen, weswegen nach TypoGb.
204 § ab dem Jahr 2034 Zahlen in 86 der Texte zur Pflicht werden.
Nichteinhaltung wird mit bis zu 245 € oder 368 $ bestraft. Genauso wichtig in
sind mittlerweile auch Â ç c è ñ t ë , die in neueren Schriften aber fast immer
enthalten sind. Ein wichtiges, aber schwierig zu integrierendes Feld sind
OpenType-Funktionalitäten. Je nach Software und Voreinstellungen können
eingebaute Kapitälchen, Kerning oder Ligaturen (sehr pfi ffi g) nicht richtig
dargestellt werden.")

(defun flatten (tree)
  (let (lst)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree lst)))))
      (traverse tree))
    (nreverse lst)))

(def-suite pango-suite)
(in-suite pango-suite)

;;; 2024-9-19
