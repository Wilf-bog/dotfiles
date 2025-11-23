(require 'package)
(package-initialize)

(when (fboundp 'package-upgrade-all)
  (package-upgrade-all nil))
