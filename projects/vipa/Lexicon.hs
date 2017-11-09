module Lexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat =  Past | Futu  | Prog | Pres | Moda | Nega | Nom | AccOrDat
          deriving (Eq,Show,Ord)

lexicon :: String -> [Cat]


lexicon "chúng"   	= [Cat "chung"   	"NP" [] []]
lexicon "họ"  		= [Cat "ho"  		"NP" [] []]

lexicon "bống"    	= [Cat "bong"  		"CN" [] []]
lexicon "bụt"    	= [Cat "but"  		"CN" [] []]
lexicon "cám"    	= [Cat "cam"  		"CN" [] []]
lexicon "tấm"    	= [Cat "tam"  		"CN" [] []]
lexicon "hai"   	= [Cat "hai"   		"CD" [] []]
lexicon "đôi"     	= [Cat "doi"     	"MW" [] []]

lexicon "người"		= [Cat "nguoi" 		"CN" [] []]
lexicon "bố"   		= [Cat "bo"   		"CN" [] []]
lexicon "cha"   	= [Cat "cha"   		"CN" [] []]
lexicon "mẹ"   		= [Cat "me"   		"CN" [] []]
lexicon "mẹ-ghẻ"	= [Cat "me-ghe"		"CN" [] []]
lexicon "con"   	= [Cat "con"   		"CN" [] []]
lexicon "chị"  		= [Cat "chi"   		"CN" [] []]
lexicon "em"   		= [Cat "em"   		"CN" [] []]
lexicon "bạn"		= [Cat "ban"		"CN" [] []]
lexicon "vua"  		= [Cat "vua"   		"CN" [] []]
lexicon "hoàng-hậu" = [Cat "hoang-hau"	"CN" [] []]
lexicon "lời"  		= [Cat "loi"   		"CN" [] []]
lexicon "cá" 		= [Cat "ca"   		"CN" [] []]
lexicon "chim" 		= [Cat "chim"  		"CN" [] []]
lexicon "xác"  		= [Cat "xac"   		"CN" [] []]
lexicon "xương"		= [Cat "xuong" 		"CN" [] []]
lexicon "nước" 		= [Cat "nuoc"  		"CN" [] []]
lexicon "mắm"  		= [Cat "mam"   		"CN" [] []]
lexicon "nước-sôi"	= [Cat "nuoc-soi"	"CN" [] []]
lexicon "gạo"   	= [Cat "gao"   		"CN" [] []]
lexicon "thóc" 		= [Cat "thoc"  		"CN" [] []]
lexicon "áo"   		= [Cat "ao"   		"CN" [] []]
lexicon "quần" 		= [Cat "quan"  		"CN" [] []]
lexicon "giày" 		= [Cat "giay"  		"CN" [] []]
lexicon "lễ"   		= [Cat "le"   		"CN" [] []]
lexicon "hội"   	= [Cat "hoi"   		"CN" [] []]
lexicon "giỗ"  		= [Cat "gio"   		"CN" [] []]
lexicon "cây"  		= [Cat "cay"   		"CN" [] []]
lexicon "quả"  		= [Cat "qua"   		"CN" [] []]
lexicon "cau"  		= [Cat "cau"   		"CN" [] []]
lexicon "thị"  		= [Cat "thi"   		"CN" [] []]
lexicon "chân" 		= [Cat "chan"  		"CN" [] []]
lexicon "giường"	= [Cat "giuong"		"CN" [] []]
lexicon "phòng"		= [Cat "phong" 		"CN" [] []]
lexicon "nhà"  		= [Cat "nha"   		"CN" [] []]
lexicon "cung" 		= [Cat "cung"  		"CN" [] []]
lexicon "ao"   		= [Cat "ao"   		"CN" [] []]

lexicon "cùng-cha-khác-mẹ"	= [Cat "cung-cha-khac-me"		"ADJ" [] []]
lexicon "đẹp"		= [Cat "dep" 		"ADJ" [] []]
lexicon "ngon"  	= [Cat "ngon"  		"ADJ" [] []]

lexicon "đã"	    = [Cat "da"		    "AUX" [Past] []]
lexicon "đang"	    = [Cat "dang"	    "AUX" [Prog] []]
lexicon "sẽ"	    = [Cat "se"		    "AUX" [Futu] []]
lexicon "không-thể" = [Cat "khong-the"  "AUX" [Moda] []]
lexicon "không"	    = [Cat "khong"	    "AUX" [Nega] []]
lexicon "chỉ"	    = [Cat "chi"	    "AUX" [Pres] []]
lexicon "lại"	    = [Cat "lai"	    "AUX" [Pres] []]
lexicon "rất"	    = [Cat "rat"	    "AUX" [Pres] []]
lexicon "suốt-ngày" = [Cat "suot-ngay"  "AUX" [Pres] []]

lexicon "trong"		= [Cat "trong" 		"PREP" [] []]
lexicon "cho"   	= [Cat "cho"   		"PREP" [] []]
lexicon "từ" 		= [Cat "tu" 		"PREP" [] []]
lexicon "dưới"	 	= [Cat "duoi"   	"PREP" [] []]
lexicon "với" 		= [Cat "voi" 		"PREP" [] []]
lexicon "bên" 		= [Cat "ben" 		"PREP" [] []]

lexicon "lên" 		= [Cat "len" 		"AUG" [] []]
lexicon "ra" 		= [Cat "ra" 		"AUG" [] []]
lexicon "thấy" 		= [Cat "thay" 		"AUG" [] []]
lexicon "được" 		= [Cat "duoc" 		"AUG" [] []]
lexicon "vừa" 		= [Cat "vua" 		"AUG" [] []]
lexicon "xuống"		= [Cat "xuong" 		"AUG" [] []]
lexicon "vào" 		= [Cat "vao" 		"AUG" [] []]

-- INTRANS (8)
lexicon "ăn"		= [Cat "an"			"VP" [] []]
lexicon "chết"		= [Cat "chet"		"VP" [] []]
lexicon "chết-đuối"	= [Cat "chet-duoi"	"VP" [] []]
lexicon "cô-đơn"	= [Cat "co-don"		"VP" [] []]
lexicon "đi"		= [Cat "di"			"VP" [] []]
lexicon "ghen-tức"	= [Cat "ghen-tuc"	"VP" [] []]
lexicon "khóc"		= [Cat "khoc"		"VP" [] []]
lexicon "mất"		= [Cat "mat"		"VP" [] []]
lexicon "tức-giận"	= [Cat "tuc-gian"	"VP" [] []]

-- NP (16)
lexicon "là"		= [Cat "la"			"VP" [] [Cat "_" "NP" [] []]]
lexicon "hành-hạ"	= [Cat "hanh-ha"	"VP" [] [Cat "_" "NP" [] []]]
lexicon "làm-thịt"	= [Cat "lam-thit"	"VP" [] [Cat "_" "NP" [] []]]
lexicon "mở"		= [Cat "mo"			"VP" [] [Cat "_" "NP" [] []]]
lexicon "dự"		= [Cat "du"			"VP" [] [Cat "_" "NP" [] []]]
lexicon "trở-thành"	= [Cat "tro-thanh"	"VP" [] [Cat "_" "NP" [] []]]
lexicon "về"		= [Cat "ve"			"VP" [] [Cat "_" "NP" [] []]]
lexicon "nghe"		= [Cat "nghe"		"VP" [] [Cat "_" "NP" [] []]]
lexicon "hái"		= [Cat "hai"		"VP" [] [Cat "_" "NP" [] []]]
lexicon "chặt"		= [Cat "chat"		"VP" [] [Cat "_" "NP" [] []]]
lexicon "giết"		= [Cat "giet"		"VP" [] [Cat "_" "NP" [] []]]
lexicon "thăm"		= [Cat "tham"		"VP" [] [Cat "_" "NP" [] []]]
lexicon "gặp"		= [Cat "gap"		"VP" [] [Cat "_" "NP" [] []]]
lexicon "trở-về"	= [Cat "tro-ve"		"VP" [] [Cat "_" "NP" [] []]]
lexicon "dội"		= [Cat "doi"		"VP" [] [Cat "_" "NP" [] []]]
lexicon "mặc"		= [Cat "mac"		"VP" [] [Cat "_" "NP" [] []]]

-- AP (1)
lexicon "khen"		= [Cat "khen"		"VP" [] [Cat "_" "ADJ" [] []]]

-- PP (1)
lexicon "ở"			= [Cat "o"			"VP" [] [Cat "_" "PP" [] []]]
lexicon "quấn-quýt"	= [Cat "quan-quyt"	"VP" [] [Cat "_" "PP" [] []]]

-- INTRANS / PP (1)
lexicon "xem"		= [Cat "xem"		"VP" [] [],
					   Cat "xem"		"VP" [] [Cat "_" "PP" [] []]]

-- AUG (2)
lexicon "hiện"		= [Cat "hien"		"VP" [] [Cat "_" "AUG" [] []]]
lexicon "trèo"		= [Cat "treo"		"VP" [] [Cat "_" "AUG" [] []]]

-- NP VP (6)
lexicon "muốn"		= [Cat "muon"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]
lexicon "sai"		= [Cat "sai"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]
lexicon "nhờ"		= [Cat "nho"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]
lexicon "bảo"		= [Cat "bao"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]
lexicon "linh-cảm"	= [Cat "linh-cam"	"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]
lexicon "phong"		= [Cat "phong"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]

-- NP / (NP VP) (4)
lexicon "có"		= [Cat "co"			"VP" [] [Cat "_" "NP" [] []],
					   Cat "co"			"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]
lexicon "hóa-thành"	= [Cat "hoa-thanh"	"VP" [] [Cat "_" "NP" [] []],
					   Cat "hoa-thanh"	"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]
lexicon "làm"		= [Cat "lam"		"VP" [] [Cat "_" "NP" [] []],
					   Cat "lam"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]
lexicon "giúp"		= [Cat "giup"		"VP" [] [Cat "_" "NP" [] []],
					   Cat "giup"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "VP" [] []]]

-- NP PP (4)
lexicon "chôn"		= [Cat "chon"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "PP" [] []]]
lexicon "trộn"		= [Cat "tron"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "PP" [] []]]
lexicon "làm-rơi"	= [Cat "lam-roi"	"VP" [] [Cat "_" "NP" [] [], Cat "_" "PP" [] []]]
lexicon "tặng"		= [Cat "tang"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "PP" [] []]]

-- NP AUG (2)
lexicon "tách-riêng"= [Cat "tach-rieng"	"VP" [] [Cat "_" "NP" [] [], Cat "_" "AUG" [] []]]
lexicon "đào"		= [Cat "dao"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "AUG" [] []]]

-- AUG NP (5)
lexicon "phát-hiện"	= [Cat "phat-hien"	"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]
lexicon "nhặt"		= [Cat "nhat"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]
lexicon "ướm"		= [Cat "uom"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]
lexicon "rơi"		= [Cat "roi"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]
lexicon "bay"		= [Cat "bay"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]

-- NP / (AUG NP) (2)
lexicon "đem"		= [Cat "dem"		"VP" [] [Cat "_" "NP" [] []],
					   Cat "dem"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]
lexicon "tìm"		= [Cat "tim"		"VP" [] [Cat "_" "NP" [] []],
					   Cat "tim"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]
-- AUG PP (1)
lexicon "chui"		= [Cat "chui"		"VP" [] [Cat "_" "AUG" [] [], Cat "_" "PP" [] []]]

-- NP AUG NP (1)
lexicon "thế"		= [Cat "the"		"VP" [] [Cat "_" "NP" [] [], Cat "_" "AUG" [] [], Cat "_" "NP" [] []]]

lexicon _ = []

{-
Tấm Cám là hai chị em cùng-cha-khác-mẹ
Bố Tấm đã mất
Tấm ở với mẹ con Cám
Họ suốt-ngày hành-hạ Tấm
Tấm rất cô-đơn

Tấm chỉ có cá bống làm bạn
Cám phát-hiện ra Bống
Cám làm-thịt Bống
Tấm không tìm thấy Bống
Tấm khóc

Bụt hiện lên
Bụt giúp Tấm tìm xương Bống
Tấm chôn xương Bống dưới chân giường
Vua mở hội
Mẹ con Cám đi xem

Họ không muốn Tấm đi
Mẹ-ghẻ trộn thóc với gạo
Mẹ sai Tấm tách-riêng chúng ra
Tấm không-thể đi dự hội
Tấm khóc

Bụt lại hiện lên
Bụt nhờ chim giúp Tấm
Tấm không có quần áo đẹp đi dự hội
Bụt bảo Tấm đào xương bống lên
Xương bống hóa-thành quần áo đẹp

Tấm mặc quần áo đi dự hội
Tấm làm-rơi giày trong lễ hội
Vua nhặt được đôi giày
Tấm ướm vừa giày
Tấm trở-thành hoàng-hậu làm mẹ con Cám tức-giận

Tấm về giỗ cha
Tấm nghe lời mẹ-ghẻ trèo lên hái cau
Mẹ-ghẻ chặt cây cau
Tấm rơi xuống ao chết-đuối
Cám thế Tấm vào cung làm hoàng-hậu

Tấm hóa-thành chim bay vào cung
Vua linh-cảm chim là Tấm
Vua quấn-quýt bên chim
Cám ghen-tức
Cám giết chim

Chim chết hóa-thành cây thị
Vua hái quả thị đem vào phòng
Cám về nhà thăm mẹ
Tấm chui ra từ quả thị gặp vua
Vua lại phong Tấm làm hoàng-hậu

Cám trở-về cung
Tấm sai người dội nước-sôi giết Cám
Tấm đem xác Cám làm nước mắm
Tấm tặng mắm cho mẹ-ghẻ
Mẹ-ghẻ ăn khen ngon
-}