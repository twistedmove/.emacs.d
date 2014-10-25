(require 'comint)

(defvar xmidas-file-path "/usr/local/bin/tcsh"
  "Path to the program used by `run-xmidas'")

(defvar xmidas-arguments '("-c" "source ~/.emacs.d/nispio/xmidas")
  "Commandline arguments to pass to `xmidas'")

;; (setq xmidas-file-path "/usr/local/bin/tcsh")
(setq xmidas-arguments '("-c" "source ~/software/bin/xmidas"))

(defvar xmidas-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; Define keys
    ;(define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-xmidas'")

(defvar xmidas-prompt-regexp "^[PSX]-Midas>"
  "Prompt for `run-xmidas'.")

(defun run-xmidas ()
  "Run an inferior instance of `xmidas' inside Emacs."
  (interactive)
  (let* ((xmidas-program xmidas-file-path)
         (buffer (comint-check-proc "X-Midas")))
    ;; pop to the "*X-Midas*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'xmidas-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*X-Midas*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "X-Midas" buffer
             xmidas-program nil xmidas-arguments)
      (xmidas-mode))))

(defun xmidas--initialize ()
  "Helper function to initialize X-Midas"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))
 
(define-derived-mode xmidas-mode comint-mode "X-Midas"
  "Major mode for `run-xmidas'.
 
\\<xmidas-mode-map>"
  nil "X-Midas"
  ;; this sets up the prompt
  (setq comint-prompt-regexp xmidas-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(xmidas-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) xmidas-prompt-regexp))
 
;; this has to be done in a hook. grumble grumble.
(add-hook 'xmidas-mode-hook 'xmidas--initialize)

(set (make-local-variable 'font-lock-defaults) '(xmidas-font-lock-keywords t))

(defvar emacspipe-regexp "tmp\\.[A-Za-z0-9]\\{10\\}"
  "Regexp describing the name of temp buffers")

(add-to-list 'auto-mode-alist
  (cons emacspipe-regexp 'view-mode))

(defconst xmidas-keywords
  '("abscissa" "fastfilter" "parse" "ttcmd" "acqmod" "fcalculator" "passfilt"
"ttuner" "add" "fcreate" "pause" "tubiquitous" "advisory" "fft" "peakpick"
"tutor" "amfsync" "fftsize" "phase" "twodthin" "ask" "filelist" "pipe" "typeof"
"attach" "files" "polar" "ubiquitous" "audiocd" "filetransp" "polltc" "ungroup"
"auxiliary" "filt3000" "polyeval" "uniform" "beep" "filtdec" "polyfit" "units"
"biexpsvi" "find" "polyphase" "universe" "bitmask" "firhlbrt" "prifunc"
"unprotect" "blockavg" "firparks" "prithin" "unpyenv" "blue2xtalk" "firwind"
"protect" "unwrap" "boolean" "fname" "pulse" "update" "break" "forall" "py"
"upsample" "browse" "fsksim" "pyenv" "upzero" "buildopt" "fstatistics" "qsort"
"userid" "calculator" "fsva" "ramp" "value" "call" "fsvi" "random" "verify"
"category" "gate" "readid" "vfiles" "checkout" "genexplain" "recall" "vlist"
"checkstat" "genleapseconds" "reformat" "warning" "circcorl" "genmsginc"
"release" "waveform" "clkcmd" "genpktsinc" "remove" "where" "cntlthin"
"getdisplayinfo" "repack4000" "while" "cnvt_martes" "getvalue" "reset"
"whitenoise" "comment" "go" "results" "widget" "compare" "goto" "return" "window"
"concatenate" "group" "rmif" "wrap" "configure" "hardlimit" "rqfshift" "writeid"
"conjugate" "headermod" "rsm" "xatmplot" "constant" "help" "rthistogram"
"xaudiocd" "continue" "hilbert" "rtliss" "xbhost" "contour" "histogram" "rtlissd"
"xblib" "control" "history" "sam" "xcall" "convert" "homepath" "samdriver"
"xcfgedit" "convolve" "homogenize" "sarray" "xcntrl" "copyheader" "hpstep" "save"
"xcolormap" "correlate" "hshftdec" "say" "xcontrol" "cyclo4" "hw" "sbtuner"
"xdatalist" "darray" "hwconfig" "sd350" "xenvironment" "dataimport" "hwdriver"
"sedit" "xfpp" "datalist" "if" "setnode" "xgeo" "dates" "imfft" "setseed"
"xgeoevent" "dblog" "import" "sincosine" "xgeoplot" "debug" "inactive" "sinkfile"
"xhelp" "decatenate" "info" "sinkmsg" "xmbackup" "defaults" "interleave"
"sinksam" "xmbld" "demos" "interpolate" "sinksb" "xmblib" "demux" "invfft"
"sinksocket" "xmbopt" "detach" "jump" "skip" "xmbrpm" "detect" "keyword" "slave"
"xmcheckproc" "digtc" "label" "smooth" "xmdir" "divide" "limit" "socketmsg"
"xmessage" "dpco" "lissajous" "sounder" "xmkill" "dpipe" "loadconstants"
"sourcefile" "xmlistprocs" "dr300_ttuner" "location" "sourcemsg" "xmopt" "dsm"
"logarithm" "sourcesam" "xmpath" "dsmclient" "loop" "sourcesb" "xmproclist"
"dsmodule" "lrs" "sourcesocket" "xmpy" "dt202_ttuner" "lrs64" "spectra"
"xmremotes" "dt300_ttuner" "magnitude" "speedometer" "xmrestore" "dt700_ttuner"
"mapman" "splice" "xmsglog" "dual5000" "marray" "split" "xmultipanel" "dynzoom"
"mask" "statevector" "xmvalue" "editlist" "master" "statistics" "xmver" "else"
"maxmin" "status" "xp3d" "elseif" "mdx" "step" "xpanel" "emergency" "medianfilt"
"stl3116_set" "xpc" "endif" "medit" "subshell" "xpipe" "endloop" "menu"
"subtract" "xpipemonitor" "endmode" "mergefile" "surfdemo" "xplot" "endsuspend"
"message" "svicmd" "xqmonitor" "endwhile" "mfft" "switch" "xraster" "envelope"
"mfile" "syncplot" "xrtdisplay" "environment" "mlist" "syntax" "xrtplot" "erase"
"modulo" "syscon" "xrtraster" "ermif" "morph" "t6filter" "xrtsamview" "error"
"morphfilt" "tbl" "xrtsurface" "exit" "mpoly" "tblfilter" "xscape" "explain"
"msgmask" "tccmd" "xsearch" "export" "mtranslate" "thin" "xtalk2blue" "extract"
"mult2000" "timecode" "xtext" "f3000" "multiply" "timer" "xtutor" "f6000" "mux"
"timex" "xview" "faketc" "newrate" "track" "xwindow" "fanin" "noop" "transfft"
"zpipe" "fanout" "normalize" "transform" "fastcorl" "os" "transpose"
"fasterfilter" "parray" "trap" "X-Midas>" "abscissa" "fastfilter" "parse" "ttcmd"
"acqmod" "fcalculator" "passfilt" "ttuner" "add" "fcreate" "pause" "tubiquitous"
"advisory" "fft" "peakpick" "tutor" "amfsync" "fftsize" "phase" "twodthin" "ask"
"filelist" "pipe" "typeof" "attach" "files" "polar" "ubiquitous" "audiocd"
"filetransp" "polltc" "ungroup" "auxiliary" "filt3000" "polyeval" "uniform"
"beep" "filtdec" "polyfit" "units" "biexpsvi" "find" "polyphase" "universe"
"bitmask" "firhlbrt" "prifunc" "unprotect" "blockavg" "firparks" "prithin"
"unpyenv" "blue2xtalk" "firwind" "protect" "unwrap" "boolean" "fname" "pulse"
"update" "break" "forall" "py" "upsample" "browse" "fsksim" "pyenv" "upzero"
"buildopt" "fstatistics" "qsort" "userid" "calculator" "fsva" "ramp" "value"
"call" "fsvi" "random" "verify" "category" "gate" "readid" "vfiles" "checkout"
"genexplain" "recall" "vlist" "checkstat" "genleapseconds" "reformat" "warning"
"circcorl" "genmsginc" "release" "waveform" "clkcmd" "genpktsinc" "remove"
"where" "cntlthin" "getdisplayinfo" "repack4000" "while" "cnvt_martes" "getvalue"
"reset" "whitenoise" "comment" "go" "results" "widget" "compare" "goto" "return"
"window" "concatenate" "group" "rmif" "wrap" "configure" "hardlimit" "rqfshift"
"writeid" "conjugate" "headermod" "rsm" "xatmplot" "constant" "help"
"rthistogram" "xaudiocd" "continue" "hilbert" "rtliss" "xbhost" "contour"
"histogram" "rtlissd" "xblib" "control" "history" "sam" "xcall" "convert"
"homepath" "samdriver" "xcfgedit" "convolve" "homogenize" "sarray" "xcntrl"
"copyheader" "hpstep" "save" "xcolormap" "correlate" "hshftdec" "say" "xcontrol"
"cyclo4" "hw" "sbtuner" "xdatalist" "darray" "hwconfig" "sd350" "xenvironment"
"dataimport" "hwdriver" "sedit" "xfpp" "datalist" "if" "setnode" "xgeo" "dates"
"imfft" "setseed" "xgeoevent" "dblog" "import" "sincosine" "xgeoplot" "debug"
"inactive" "sinkfile" "xhelp" "decatenate" "info" "sinkmsg" "xmbackup" "defaults"
"interleave" "sinksam" "xmbld" "demos" "interpolate" "sinksb" "xmblib" "demux"
"invfft" "sinksocket" "xmbopt" "detach" "jump" "skip" "xmbrpm" "detect" "keyword"
"slave" "xmcheckproc" "digtc" "label" "smooth" "xmdir" "divide" "limit"
"socketmsg" "xmessage" "dpco" "lissajous" "sounder" "xmkill" "dpipe"
"loadconstants" "sourcefile" "xmlistprocs" "dr300_ttuner" "location" "sourcemsg"
"xmopt" "dsm" "logarithm" "sourcesam" "xmpath" "dsmclient" "loop" "sourcesb"
"xmproclist" "dsmodule" "lrs" "sourcesocket" "xmpy" "dt202_ttuner" "lrs64"
"spectra" "xmremotes" "dt300_ttuner" "magnitude" "speedometer" "xmrestore"
"dt700_ttuner" "mapman" "splice" "xmsglog" "dual5000" "marray" "split"
"xmultipanel" "dynzoom" "mask" "statevector" "xmvalue" "editlist" "master"
"statistics" "xmver" "else" "maxmin" "status" "xp3d" "elseif" "mdx" "step"
"xpanel" "emergency" "medianfilt" "stl3116_set" "xpc" "endif" "medit" "subshell"
"xpipe" "endloop" "menu" "subtract" "xpipemonitor" "endmode" "mergefile"
"surfdemo" "xplot" "endsuspend" "message" "svicmd" "xqmonitor" "endwhile" "mfft"
"switch" "xraster" "envelope" "mfile" "syncplot" "xrtdisplay" "environment"
"mlist" "syntax" "xrtplot" "erase" "modulo" "syscon" "xrtraster" "ermif" "morph"
"t6filter" "xrtsamview" "error" "morphfilt" "tbl" "xrtsurface" "exit" "mpoly"
"tblfilter" "xscape" "explain" "msgmask" "tccmd" "xsearch" "export" "mtranslate"
"thin" "xtalk2blue" "extract" "mult2000" "timecode" "xtext" "f3000" "multiply"
"timer" "xtutor" "f6000" "mux" "timex" "xview" "faketc" "newrate" "track"
"xwindow" "fanin" "noop" "transfft" "zpipe" "fanout" "normalize" "transform"
"fastcorl" "os" "transpose" "fasterfilter" "parray" "trap"))

(defvar xmidas-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt xmidas-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `xmidas-mode'.")

(provide 'nispio/xmidas)
