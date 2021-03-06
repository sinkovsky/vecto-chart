(in-package #:cl-user)

(defpackage :vecto-chart-asd
  (:use #:cl #:asdf))

(in-package #:vecto-chart-asd)

(defsystem "vecto-chart"
  :description "vecto-chart: charting library based on Vecto"
  :version "0.1"
  :author "Sergey Sinkovskiy <glorybox.away@gmail.com>"
  :license "Public Domain"
  :depends-on ("vecto" "flexi-streams")
  :components ((:static-file "vecto-chart.asd")
			   (:module :src
						:components
						((:file "vecto-chart")))))

