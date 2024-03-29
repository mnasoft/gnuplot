;;;; ./src/color-names.lisp

(in-package :gnuplot)

(defparameter *color-names-list*
  '(("white"              #xffffff)
    ("black"              #x000000)
    ("dark-grey"          #xa0a0a0)
    ("red"                #xff000)
    ("web-green"          #x00c000)
    ("web-blue"           #x0080ff)
    ("dark-magenta"       #xc000ff)
    ("dark-cyan"          #x00eeee)
    ("dark-orange"        #xc04000)
    ("dark-yellow"        #xc8c800)
    ("royalblue"          #x4169e1)
    ("goldenrod"          #xffc020)
    ("dark-spring-green"  #x008040)
    ("purple"             #xc080ff)
    ("steelblue"          #x306080)
    ("dark-red"           #x8b0000)
    ("dark-chartreuse"    #x408000)
    ("orchid"             #xff80ff)
    ("aquamarine"         #x7fffd4)
    ("brown"              #xa52a2a)
    ("yellow"             #xffff00)
    ("turquoise"          #x40e0d0)
    ("grey0"              #x000000)
    ("grey10"             #x1a1a1a)
    ("grey20"             #x333333)
    ("grey30"             #x4d4d4d)
    ("grey40"             #x666666)
    ("grey50"             #x7f7f7f)
    ("grey60"             #x999999)
    ("grey70"             #xb3b3b3)
    ("grey"               #xc0c0c0)
    ("grey80"             #xcccccc)
    ("grey90"             #xe5e5e5)
    ("grey100"            #xffffff)
    ("light-red"          #xf03232)
    ("light-green"        #x90ee90)
    ("light-blue"         #xadd8e6)
    ("light-magenta"      #xf055f0)
    ("light-cyan"         #xe0ffff)
    ("light-goldenrod"    #xeedd82)
    ("light-pink"         #xffb6c1)
    ("light-turquoise"    #xafeeee)
    ("gold"               #xffd700)
    ("green"              #x00ff00)
    ("dark-green"         #x006400)
    ("spring-green"       #x00ff7f)
    ("forest-green"       #x228b22)
    ("sea-green"          #x2e8b57)
    ("blue"               #x0000ff)
    ("dark-blue"          #x00008b)
    ("midnight-blue"      #x191970)
    ("navy"               #x000080)
    ("medium-blue"        #x0000cd)
    ("skyblue"            #x87ceeb)
    ("cyan"               #x00ffff)
    ("magenta"            #xff00ff)
    ("dark-turquoise"     #x00ced1)
    ("dark-pink"          #xff1493)
    ("coral"              #xff7f50)
    ("light-coral"        #xf08080)
    ("orange-red"         #xff4500)
    ("salmon"             #xfa8072)
    ("dark-salmon"        #xe9967a)
    ("khaki"              #xf0e68c)
    ("dark-khaki"         #xbdb76b)
    ("dark-goldenrod"     #xb8860b)
    ("beige"              #xf5f5dc)
    ("olive"              #xa08020)
    ("orange"             #xffa500)
    ("violet"             #xee82ee)
    ("dark-violet"        #x9400d3)
    ("plum"               #xdda0dd)
    ("dark-plum"          #x905040)
    ("dark-olivegreen"    #x556b2f)
    ("orangered4"         #x801400)
    ("brown4"             #x801414)
    ("sienna4"            #x804014)
    ("orchid4"            #x804080)
    ("mediumpurple3"      #x8060c0)
    ("slateblue1"         #x8060ff)
    ("yellow4"            #x808000)
    ("sienna1"            #xff8040)
    ("tan1"               #xffa040)
    ("sandybrown"         #xffa060)
    ("light-salmon"       #xffa070)
    ("pink"               #xffc0c0)
    ("khaki1"             #xffff80)
    ("lemonchiffon"       #xffffc0)
    ("bisque"             #xcdb79e)
    ("honeydew"           #xf0fff0)
    ("slategrey"          #xa0b6cd)
    ("seagreen"           #xc1ffc1)
    ("antiquewhite"       #xcdc0b0)
    ("chartreuse"         #x7cff40)
    ("greenyellow"        #xa0ff20)
    ("gray"               #xbebebe)
    ("light-gray"         #xd3d3d3)
    ("light-grey"         #xd3d3d3)
    ("dark-gray"          #xa0a0a0)
    ("slategray"          #xa0b6cd)
    ("gray0"              #x000000)
    ("gray10"             #x1a1a1a)
    ("gray20"             #x333333)
    ("gray30"             #x4d4d4d)
    ("gray40"             #x666666)
    ("gray50"             #x7f7f7f)
    ("gray60"             #x999999)
    ("gray70"             #xb3b3b3)
    ("gray80"             #xcccccc)
    ("gray90"             #xe5e5e5)
    ("gray100"            #xffffff)))

(defparameter *color-names* (make-hash-table :test #'equal)
  "Поределяет соответствие между именем цвета и его RGB-значением.")

(mnas-hash-table:populate *color-names* *color-names-list* 0 1)
