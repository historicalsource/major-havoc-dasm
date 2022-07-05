;********************************************
;* Major Havoc Vector ROM Overlay Page 2    *
;********************************************
	.title "TWOV2"
;********************************************
;* Includes:                                *
;********************************************
#include "vector.ah"	;For the various vector macros

;********************************************
;* Program:                                 *
;********************************************
	.org $6000
		
page = 2

		.dw $E8BB 	;3rd Vector ROM Page
		
;**************************************
	.sbttl "Extra Life"
;**************************************
live7 = $60|page

live		.db $23,$40,$21,$44,$21,$5C,$23,$40,$3E,$47,$24,$40,$3B,$43,$21,$42
		.db $3E,$41,$3E,$5F,$21,$5E,$3B,$5D,$24,$40,$3E,$59,$0C,$40,$00,$C0

;**************************************
	.sbttl "Maze Pieces"
;**************************************
maz7 	 = $60|page
mapdt7 = $60|page

maze1		.db $00,$00,$FF,$20,$00,$C0
maze2		.db $00,$00,$7F,$20,$81,$1F,$00,$20,$7F,$00,$7F,$00,$00,$C0
maze3		.db $00,$00,$7F,$20,$7F,$00,$00,$20,$81,$1F,$7F,$00,$00,$C0
maze4		.db $7F,$00,$7F,$00,$81,$1F,$00,$20,$00,$00,$7F,$20,$00,$C0
maze5		.db $81,$1F,$7F,$00,$7F,$00,$00,$20,$00,$00,$7F,$20,$00,$C0
maze6		.db $81,$1F,$7F,$00,$FF,$00,$00,$20,$81,$1F,$7F,$00,$00,$C0
maze7		.db $00,$00,$FF,$00,$00,$C0
mape1		.db $28,$40,$00,$C0
mape2		.db $24,$40,$20,$5C,$04,$44,$00,$C0
mape3		.db $24,$40,$20,$44,$04,$5C,$00,$C0
mape4		.db $04,$44,$20,$5C,$24,$40,$00,$C0
mape5		.db $04,$5C,$20,$44,$24,$40,$00,$C0
mape6		.db $04,$5C,$20,$48,$04,$5C,$00,$C0
mape7		.db $08,$40,$00,$C0
radsign	.db $0A,$40,$11,$00,$F6,$3F,$36,$40,$EF,$1F,$F6,$3F,$EF,$1F,$0A,$20
		.db $2A,$40,$11,$00,$0A,$20,$09,$00,$F1,$1F,$FB,$62,$3B,$57,$09,$00
		.db $FB,$3F,$2A,$40,$F7,$1F,$FB,$3F,$3B,$49,$25,$40,$00,$C0
		
mapdot	.db $23,$40,$1F,$5E,$20,$43,$00,$C0

;**************************************
	.sbttl "Reactor Rods"
;**************************************
rods7 = $60|page

rod0		.db $EC,$1F,$05,$00,$1B,$00,$F6,$3F,$E5,$1F,$0F,$00,$20,$4F,$E2,$1F
		.db $05,$00,$1B,$00,$0A,$20,$D6,$1F,$02,$00,$A2,$F0
rod1		.db $F1,$1F,$04,$00,$1B,$00,$F6,$3F,$E5,$1F,$10,$00,$20,$4F,$03,$51
		.db $1B,$00,$0A,$20,$D1,$1F,$02,$00,$A2,$F0
rod2		.db $F6,$1F,$03,$00,$1B,$00,$F6,$3F,$E5,$1F,$11,$00,$20,$4F,$E2,$1F
		.db $07,$00,$1B,$00,$0A,$20,$CC,$1F,$01,$00,$A2,$F0
rod3		.db $FB,$1F,$02,$00,$1B,$00,$F6,$3F,$E5,$1F,$12,$00,$20,$4F,$04,$51
		.db $1B,$00,$0A,$20,$C7,$1F,$00,$00,$00,$00,$05,$20,$FB,$1F,$00,$20
		.db $00,$00,$FD,$3F,$16,$45,$00,$C0
		
;**************************************
	.sbttl "Robot/Reactor Parts"
;**************************************
body7 = $60|page
gun7  = $60|page	;Use Robot head for star castle gun

body		.db $1B,$4F,$2A,$40,$D8,$1F,$0A,$20,$3B,$5B,$36,$40,$3B,$45,$28
		.db $00,$0A,$20,$00,$C0
bodyt		.db $1E,$00,$FB,$1F,$2A,$40,$D8,$1F,$05,$20,$3B,$5B,$05,$00,$E7,$3F
		.db $0A,$00,$FB,$3F,$23,$00,$0F,$20,$00,$C0
		
head0		.db $25,$45,$25,$5B,$36,$40,$00,$C0
head1		.db $08,$00,$11,$20,$3C,$42,$F4,$1F,$F7,$3F,$00,$C0
head2		.db $11,$00,$08,$20,$3A,$5E,$F3,$1F,$04,$20,$00,$C0
head3		.db $20,$4A,$3B,$5B,$25,$5B,$00,$C0
head4		.db $11,$00,$F8,$3F,$3E,$5A,$FB,$1F,$0C,$20,$00,$C0
head5		.db $08,$00,$EF,$3F,$22,$5A,$04,$00,$0D,$20,$00,$C0
head6		.db $36,$40,$25,$5B,$25,$45,$00,$C0
tail0		.db $CE,$1F,$05,$00,$F6,$1F,$FB,$3F,$2A,$40,$0A,$00,$FB,$3F,$32,$00
		.db $FB,$1F,$00,$C0
tail1		.db $CE,$1F,$FB,$1F,$FB,$1F,$FB,$3F,$FB,$1F,$0F,$20,$20,$45,$32,$00
		.db $05,$00,$00,$C0
gun0		.db $00,$C0
gun1		.db $00,$00,$FB,$1F,$05,$00,$00,$20,$25,$40,$FB,$1F,$00,$20,$00,$00,$FB,$1F,$00,$C0
gun2		.db $05,$00,$00,$20,$00,$00,$FB,$3F,$05,$00,$00,$20,$25,$40,$FB,$1F,$00,$20,$00,$00,$FB,$3F,$FB,$1F,$00,$00,$00,$C0
gun3		.db $20,$45,$00,$00,$FB,$3F,$05,$00,$00,$20,$25,$40,$FB,$1F,$00,$20,$00,$00,$FB,$3F,$00,$5B,$00,$C0
eye0		.db $F1,$1F,$FB,$1F,$FB,$1F,$06,$20,$05,$00,$05,$20,$21,$5F,$1F,$5E
		.db $FD,$1F,$00,$20,$03,$00,$F8,$1F,$FD,$1F,$00,$20,$1F,$5D,$26,$40
		.db $3B,$5F,$3F,$41,$00,$C0
eye1		.db $F1,$1F,$05,$00,$FB,$1F,$05,$20,$03,$00,$02,$20,$FC,$1F,$01,$00
		.db $FD,$1F,$00,$20,$03,$00,$F7,$1F,$FD,$1F,$00,$20,$F9,$1F,$09,$00
		.db $FD,$1F,$F6,$3F,$02,$00,$01,$20,$03,$00,$09,$20,$00,$C0
eye2		.db $F1,$1F,$05,$00,$FB,$1F,$05,$20,$21,$41,$FB,$1F,$FB,$1F,$FD,$1F
		.db $00,$20,$FB,$1F,$04,$00,$FB,$1F,$FB,$3F,$FF,$1F,$01,$20,$21,$42
		.db $00,$C0

;**************************************
	.sbttl "Max Robots"
;**************************************
robothead0	.db $20,$00,$0A,$00,$3F,$4C,$3E,$42,$3C,$40,$3E,$5E,$3F,$54,$04,$4E
		.db $20,$46,$22,$40,$20,$5A,$03,$5A,$38,$40,$24,$5C,$24,$44,$D0,$1F
		.db $F8,$1F,$00,$C0
robotbody0	.db $0C,$4A,$20,$56,$26,$40,$20,$4A,$00,$00,$B8,$3F,$20,$56,$26,$40
		.db $20,$4A,$26,$46,$2C,$40,$26,$5A,$3D,$52,$00,$00,$DC,$3F,$3D,$4E
		.db $1E,$56,$E0,$1F,$00,$20,$22,$40,$20,$5C,$3D,$40,$20,$5C,$3D,$40
		.db $20,$48,$22,$40,$20,$00,$00,$20,$0D,$5C,$20,$5A,$06,$46,$20,$5A
		.db $05,$40,$00,$00,$E0,$3F,$DC,$1F,$08,$20,$28,$40,$24,$00,$08,$20
		.db $06,$4A,$E0,$1F,$00,$20,$3E,$40,$20,$5C,$23,$40,$20,$5C,$23,$40
		.db $20,$48,$3E,$40,$20,$00,$00,$20,$00,$00,$E0,$1F,$00,$C0
robothead1	.db $20,$00,$0A,$00,$3F,$4C,$3E,$42,$3C,$40,$3E,$5E,$3F,$54,$04,$4E
		.db $20,$46,$22,$40,$20,$5A,$00,$46,$21,$5E,$20,$5C,$1A,$5A,$27,$40
		.db $3C,$5C,$06,$00,$F9,$3F,$D2,$1F,$09,$00,$00,$C0
robotbody1	.db $0A,$4A,$20,$56,$26,$40,$20,$4A,$00,$00,$C0,$3F,$20,$56,$26,$40
		.db $14,$00,$2C,$00,$3A,$46,$34,$40,$3A,$5A,$23,$52,$00,$00,$24,$20
		.db $21,$44,$02,$40,$E0,$1F,$00,$20,$3E,$40,$20,$5C,$23,$40,$20,$5C
		.db $23,$40,$20,$48,$3E,$40,$20,$00,$00,$20,$15,$5C,$20,$5A,$1A,$46
		.db $20,$5A,$1B,$40,$DC,$1F,$08,$20,$28,$40,$24,$00,$08,$20,$00,$00
		.db $E0,$3F,$1C,$4A,$E0,$1F,$00,$20,$21,$40,$20,$5C,$3F,$40,$20,$5C
		.db $3D,$40,$20,$44,$3F,$40,$20,$44,$22,$40,$20,$00,$00,$20,$0E,$40
		.db $00,$C0
robothead2	.db $20,$00,$0A,$00,$3F,$4C,$3E,$42,$3C,$40,$3E,$5E,$3F,$54,$04,$4E
		.db $20,$46,$22,$40,$20,$5A,$00,$46,$24,$5D,$DE,$1F,$02,$20,$16,$48
		.db $26,$40,$3C,$5C,$04,$00,$FB,$3F,$D4,$1F,$09,$00,$00,$C0
robotbody2	.db $08,$4A,$20,$56,$26,$40,$20,$4A,$00,$00,$C8,$3F,$20,$56,$24,$40
		.db $E0,$1F,$00,$20,$22,$40,$20,$58,$3D,$40,$20,$44,$3D,$40,$20,$44
		.db $22,$40,$20,$00,$00,$20,$14,$00,$30,$00,$3A,$46,$34,$40,$3A,$5A
		.db $23,$52,$00,$00,$24,$20,$21,$44,$E0,$1F,$00,$20,$3E,$40,$20,$5C
		.db $23,$40,$20,$5C,$23,$40,$20,$48,$3E,$40,$20,$00,$00,$20,$17,$5C
		.db $20,$5A,$1A,$46,$20,$5A,$1B,$40,$DC,$1F,$08,$20,$28,$40,$24,$00
		.db $08,$20,$00,$00,$E0,$3F,$08,$4A,$00,$C0
robothead3	.db $20,$00,$0A,$00,$3F,$4C,$3E,$42,$3C,$40,$3E,$5E,$3F,$54,$04,$4E
		.db $20,$46,$22,$40,$20,$5A,$00,$46,$25,$5B,$20,$51,$16,$48,$25,$40
		.db $3C,$5C,$03,$00,$FD,$3F,$D5,$1F,$09,$00,$00,$C0
robotbody3	.db $14,$4A,$00,$00,$30,$20,$3A,$46,$34,$40,$3A,$5A,$23,$52,$00,$00
		.db $20,$20,$04,$4E,$20,$56,$39,$40,$20,$4A,$05,$56,$E0,$1F,$00,$20
		.db $22,$40,$20,$58,$3D,$40,$20,$44,$3D,$40,$20,$44,$22,$40,$20,$00
		.db $00,$20,$1C,$5C,$20,$5A,$1A,$46,$20,$5A,$1C,$46,$20,$5A,$20,$00
		.db $F8,$1F,$20,$5A,$21,$40,$01,$5C,$20,$54,$3E,$40,$20,$5C,$23,$40
		.db $20,$5C,$23,$40,$0B,$44,$3E,$58,$38,$40,$24,$00,$F8,$3F,$2F,$40
		.db $19,$4A,$00,$C0
robothead4	.db $20,$00,$0A,$00,$3F,$4C,$3E,$42,$3C,$40,$3E,$5E,$3F,$54,$05,$4E
		.db $20,$46,$21,$40,$20,$5A,$00,$46,$27,$5B,$DE,$1F,$00,$20,$14,$4A
		.db $24,$40,$3C,$5C,$01,$00,$FF,$3F,$D6,$1F,$09,$00,$00,$C0
robotbody4	.db $06,$5C,$23,$40,$23,$4E,$3A,$46,$34,$40,$3A,$5A,$00,$00,$30,$20
		.db $18,$52,$33,$40,$3D,$4E,$0D,$40,$20,$56,$27,$40,$20,$4A,$1E,$56
		.db $E0,$1F,$00,$20,$21,$40,$21,$46,$3E,$40,$01,$5A,$21,$40,$20,$58
		.db $3D,$40,$20,$44,$3D,$40,$20,$44,$22,$40,$20,$00,$00,$20,$19,$5C
		.db $20,$5A,$1E,$46,$20,$5A,$09,$40,$34,$40,$DC,$1F,$08,$20,$28,$40
		.db $21,$44,$16,$40,$3F,$40,$20,$44,$3E,$40,$20,$44,$21,$40,$20,$00
		.db $0E,$00,$00,$C0
robothead5	.db $20,$00,$0A,$00,$3F,$4C,$3E,$42,$3C,$40,$3E,$5E,$3F,$54,$06,$4E
		.db $20,$46,$27,$5B,$DE,$1F,$00,$20,$14,$4A,$23,$40,$F8,$1F,$F9,$3F
		.db $D8,$1F,$09,$00,$00,$C0
robotbody5	.db $02,$5C,$27,$40,$23,$4E,$3A,$46,$34,$40,$3A,$5A,$23,$52,$29,$40
		.db $0C,$4E,$00,$00,$D0,$3F,$09,$40,$20,$56,$27,$40,$20,$4A,$1E,$56
		.db $E0,$1F,$00,$20,$22,$40,$20,$58,$3C,$40,$20,$44,$3E,$40,$20,$44
		.db $22,$40,$20,$00,$00,$20,$03,$5C,$20,$5A,$1A,$46,$20,$5A,$03,$40
		.db $38,$40,$DC,$1F,$08,$20,$28,$40,$24,$00,$08,$20,$3A,$40,$1E,$4A
		.db $00,$C0
robothead6	.db $20,$00,$00,$00,$3F,$4C,$3E,$42,$3C,$40,$3E,$5E,$20,$54,$05,$4E
		.db $3F,$46,$27,$5C,$DC,$1F,$04,$20,$13,$4A,$24,$40,$3C,$5C,$D8,$1F
		.db $12,$00,$00,$C0
robotbody6	.db $1F,$5B,$27,$40,$22,$4F,$3A,$46,$34,$40,$3A,$5A,$25,$51,$00,$00
		.db $0F,$20,$1E,$00,$07,$00,$21,$56,$39,$40,$3F,$4A,$05,$56,$22,$51
		.db $22,$40,$22,$58,$3C,$40,$3F,$44,$3E,$40,$3F,$44,$22,$40,$3E,$4F
		.db $04,$5B,$21,$5B,$19,$45,$21,$5B,$02,$40,$39,$40,$DC,$1F,$0C,$20
		.db $28,$40,$24,$00,$04,$20,$3A,$40,$00,$4A,$00,$C0
robothead7	.db $20,$00,$F8,$1F,$3E,$4C,$3D,$42,$3C,$40,$FC,$1F,$FC,$3F,$21,$54
		.db $04,$4E,$3F,$46,$27,$5E,$D8,$1F,$08,$20,$14,$00,$E3,$1F,$00,$00
		.db $07,$20,$3D,$5C,$D8,$1F,$1C,$00,$00,$C0
robotbody7	.db $1D,$5A,$26,$40,$20,$00,$02,$20,$3A,$46,$34,$40,$3A,$5A,$DC,$1F
		.db $0C,$20,$29,$40,$24,$00,$02,$00,$22,$56,$38,$40,$3E,$4A,$07,$56
		.db $24,$52,$22,$40,$22,$58,$3C,$40,$3F,$44,$3E,$40,$3F,$44,$22,$40
		.db $3C,$4E,$05,$5A,$21,$5C,$18,$44,$21,$5C,$04,$40,$38,$40,$DC,$1F
		.db $10,$20,$28,$40,$24,$00,$02,$20,$39,$40,$02,$4A,$00,$C0
		
levt0		.db $C0,$1F,$F8,$1F,$28,$40,$01,$5B,$36,$40,$1D,$59,$00,$00,$20,$20
		.db $02,$5C,$00,$00,$D8,$3F,$60,$00,$14,$00,$00,$C0
levt1		.db $C6,$1F,$F8,$1F,$28,$40,$00,$5C,$38,$40,$1E,$5B,$2C,$40,$03,$58
		.db $00,$00,$DC,$3F,$1F,$5E,$00,$00,$28,$20,$60,$00,$EC,$1F,$00,$C0

levt2		.db $C4,$1F,$F8,$1F,$28,$40,$00,$5C,$38,$40,$1E,$5A,$2C,$40,$04,$58
		.db $00,$00,$D8,$3F,$60,$00,$14,$00,$00,$C0
levt3		.db $C2,$1F,$F8,$1F,$28,$40,$00,$5B,$38,$40,$1D,$5A,$2E,$40,$03,$5A
		.db $00,$00,$D8,$9F,$60,$00,$14,$00,$00,$C0
		
;**************************************
	.sbttl "Ion Cannon Parts"
;**************************************
mzls7 = $60|page		

mount		.db $3C,$40,$3C,$44,$00,$00,$20,$20,$3C,$5C,$3C,$40,$00,$C0

lgun0		.db $08,$00,$20,$00,$20,$58,$00,$00,$C0,$3F,$20,$48,$00,$00,$40,$20,$F8,$1F,$E0,$1F,$00,$C0
lgun1		.db $FC,$1F,$20,$00,$3D,$58,$18,$00,$C4,$3F,$23,$48,$E8,$1F,$3C,$20,$04,$00,$E0,$1F,$00,$C0
lgun2		.db $0A,$52,$38,$5D,$3C,$00,$E8,$3F,$28,$43,$C4,$1F,$18,$20,$16,$4E,$00,$C0
lgun3		.db $20,$00,$08,$00,$C0,$1F,$00,$20,$38,$40,$40,$00,$00,$20,$28,$40,$E0,$1F,$F8,$1F,$00,$C0

brl00		.db $04,$00,$20,$00,$28,$40,$20,$5C,$38,$40,$08,$43,$28,$40,$20,$5E
		.db $38,$40,$02,$00,$D0,$1F,$00,$C0
brl01		.db $04,$00,$20,$00,$26,$40,$20,$5C,$3A,$40,$06,$43,$24,$40,$20,$5E
		.db $3C,$40,$02,$00,$D4,$1F,$00,$C0
brl02		.db $04,$00,$20,$00,$24,$40,$20,$5C,$3C,$40,$04,$43,$22,$40,$20,$5E
		.db $3E,$40,$02,$00,$D8,$1F,$00,$C0
brl10		.db $F8,$1F,$1F,$00,$27,$5D,$F8,$1F,$FD,$3F,$39,$43,$08,$40,$FA,$1F
		.db $0F,$20,$FC,$1F,$FF,$3F,$06,$00,$F1,$3F,$14,$00,$D5,$1F,$00,$C0

brl11		.db $F8,$1F,$1F,$00,$FC,$1F,$09,$20,$F8,$1F,$FD,$3F,$04,$00,$F7,$3F
		.db $06,$41,$FD,$1F,$08,$20,$3F,$5E,$03,$00,$F8,$3F,$12,$00,$DA,$1F
		.db $00,$C0
brl12		.db $F8,$1F,$1F,$00,$FE,$1F,$05,$20,$F8,$1F,$FD,$3F,$02,$00,$FB,$3F
		.db $04,$00,$07,$00,$FF,$1F,$03,$20,$FC,$1F,$FF,$3F,$01,$00,$FD,$3F
		.db $10,$00,$DE,$1F,$00,$C0
brl20		.db $E3,$1F,$10,$00,$23,$58,$FD,$1F,$F8,$3F,$3D,$48,$06,$59,$23,$59
		.db $FF,$1F,$FC,$3F,$3D,$47,$2F,$00,$F0,$1F,$00,$C0
brl21		.db $E3,$1F,$10,$00,$F4,$1F,$05,$20,$FD,$1F,$F8,$3F,$0C,$00,$FB,$3F
		.db $F6,$1F,$0B,$00,$F8,$1F,$03,$20,$FF,$1F,$FC,$3F,$08,$00,$FD,$3F
		.db $2B,$00,$F1,$1F,$00,$C0
brl22		.db $E3,$1F,$10,$00,$F8,$1F,$03,$20,$FD,$1F,$F8,$3F,$08,$00,$FD,$3F
		.db $FA,$1F,$09,$00,$21,$5E,$FF,$1F,$FC,$3F,$3F,$42,$27,$00,$F3,$1F
		.db $00,$C0
brl30		.db $E0,$1F,$04,$00,$20,$58,$3C,$40,$20,$48,$03,$58,$20,$58,$3E,$40
		.db $20,$48,$30,$00,$02,$00,$00,$C0
brl31		.db $E0,$1F,$04,$00,$20,$5A,$3C,$40,$20,$46,$03,$5A,$20,$5C,$3E,$40
		.db $20,$44,$2C,$00,$02,$00,$00,$C0
brl32		.db $E0,$1F,$04,$00,$20,$5C,$3C,$40,$20,$44,$03,$5C,$20,$5E,$3E,$40
		.db $20,$42,$28,$00,$02,$00,$00,$C0
		
		.db $22,$40,$21,$5C,$02,$58,$21,$5C,$22,$40,$00,$C0
		
laz00		.db $F8,$1F,$E0,$1F,$23,$4C,$95,$B4,$20,$00,$08,$20,$95,$B4,$20,$00
		.db $08,$20,$21,$40,$20,$5C,$14,$5C,$00,$C0
laz01		.db $F8,$1F,$E0,$1F,$20,$5C,$22,$40,$20,$00,$08,$20,$95,$B4,$20,$00
		.db $08,$20,$95,$B4,$21,$44,$15,$44,$00,$C0
laz02		.db $12,$5C,$21,$5C,$22,$40,$20,$00,$08,$20,$95,$B4,$20,$00,$08,$20
		.db $22,$40,$21,$5C,$02,$58,$21,$5C,$21,$40,$20,$44,$14,$44,$00,$C0

laz03		.db $08,$00,$E0,$1F,$20,$44,$95,$B4,$20,$00,$08,$20,$95,$B4,$20,$00
		.db $08,$20,$22,$40,$21,$5C,$15,$5C,$00,$C0
		
		.db $22,$5F,$20,$5C,$1E,$58,$20,$5C,$22,$5F,$29,$4E,$00,$C0
		
laz10		.db $04,$00,$DE,$1F,$27,$4A,$D2,$B4,$D2,$B4,$22,$5F,$20,$5C,$14,$41,$00,$C0

		.db $3D,$5E,$18,$5B,$3D,$5E,$21,$5E,$04,$00,$20,$20,$00,$C0
		
laz11 = laz01
laz12 = laz02
laz13 = laz03

laz20		.db $16,$4D,$2D,$41,$21,$5E,$E2,$B4,$21,$5E,$E2,$B4,$21,$5E,$3D,$5E,$14,$00,$EF,$1F,$00,$C0

		.db $20,$5E,$3C,$5F,$18,$5E,$3C,$5F,$20,$5E,$00,$C0

laz21 = laz01
laz22 = laz02
laz23 = laz03
		
laz30		.db $20,$00,$F8,$1F,$2C,$5D,$F4,$B4,$F8,$1F,$20,$20,$F4,$B4,$F8,$1F
		.db $20,$20,$20,$5F,$3C,$40,$1C,$4C,$00,$C0
laz31		.db $20,$00,$F8,$1F,$3C,$40,$20,$5E,$F8,$1F,$20,$20,$F4,$B4,$F8,$1F
		.db $20,$20,$F4,$B4,$24,$5F,$04,$4B,$00,$C0
laz32		.db $1C,$4E,$3C,$5F,$20,$5E,$F8,$1F,$20,$20,$F4,$B4,$F8,$1F,$20,$20
		.db $20,$5E,$3C,$5F,$18,$5E,$3C,$5F,$20,$5F,$24,$40,$04,$4C,$00,$C0
laz33		.db $20,$00,$08,$00,$24,$40,$F4,$B4,$F8,$1F,$20,$20,$F4,$B4,$F8,$1F
		.db $20,$20,$20,$5E,$3C,$5F,$1C,$4B,$00,$C0
		
dot0		.db $3E,$40,$01,$00,$00,$20,$22,$40,$01,$00,$00,$20,$3E,$40,$01,$00
		.db $00,$20,$22,$40,$FD,$1F,$00,$00,$00,$C0
dot1		.db $3D,$40,$01,$00,$00,$20,$23,$40,$01,$00,$00,$20,$3D,$40,$01,$00
		.db $00,$20,$23,$40,$FD,$1F,$00,$00,$00,$C0
beacn3
dot2		.db $3B,$40,$01,$00,$00,$20,$25,$40,$01,$00,$00,$20,$3B,$40,$01,$00
		.db $00,$20,$25,$40,$FD,$1F,$00,$00,$00,$C0

;**************************************
	.sbttl "Beacon"
;**************************************
becn7 = $60|page
		
beacn0
beacn1	.db $1B,$40,$31,$B5,$05,$40,$00,$C0
beacn2	.db $1C,$40,$3E,$B5,$04,$40,$00,$C0
beacn4	.db $04,$40,$3E,$B5,$1C,$40,$00,$C0
beacn5
beacn6	.db $05,$40,$31,$B5,$1B,$40
beacn7	.db $00,$C0

;********************************
	.sbttl "Ship Shots"
;********************************
shpsh7 = $60|page 

shipsh	.db $4B,$B5,$D6,$62,$1D,$5F,$21,$40,$01,$00,$00,$20,$3F,$40,$00,$C0

hive0		.db $86,$00,$26,$00,$00,$00,$B2,$3F,$DE,$1F,$C6,$3F,$C0,$1F,$E6,$3F
		.db $BC,$1F,$FE,$3F,$B4,$1F,$2A,$20,$E6,$1F,$32,$20,$00,$00,$3A,$20
		.db $18,$00,$34,$20,$4A,$00,$30,$20,$4C,$00,$02,$20,$3A,$00,$DE,$3F
		.db $22,$00,$CA,$3F,$E2,$1F,$DC,$3F,$20,$00,$D6,$3F,$E0,$1F,$2A,$00
		.db $BE,$1F,$FE,$3F,$CA,$1F,$1C,$20,$02,$00,$C2,$3F,$36,$00,$24,$20
		.db $C8,$1F,$1A,$00,$DA,$1F,$32,$20,$14,$00,$2E,$20,$EE,$1F,$D0,$1F
		.db $C8,$1F,$00,$20,$5E,$00,$92,$1F,$DA,$1F,$D0,$3F,$CC,$1F,$FE,$3F
		.db $34,$00,$02,$00,$16,$00,$D4,$3F,$00,$C0
		
 .org $7000


;***************************************************
;* Linear Line Growth - 3rd Quad
;***************************************************    
; hardlin is used in tw_spcmz as the base 
; pointer to this data
;hardlin
spstat7    = $60|page

webln00 vctrl(-4d,-6d,visible)      \ rtsl  ;.db $FA,$1F,$FC,$3F,$00,$C0
        vctrl(-6d,-9d,visible)      \ rtsl  ;.db $F7,$1F,$FA,$3F,$00,$C0
        vctrl(-8d,-12d,visible)     \ rtsl  ;.db $F4,$1F,$F8,$3F,$00,$C0
        vctrl(-12d,-18d,visible)    \ rtsl  ;.db $EE,$1F,$F4,$3F,$00,$C0
        vctrl(-17d,-25d,visible)    \ rtsl  ;.db $E7,$1F,$EF,$3F,$00,$C0
        vctrl(-23d,-35d,visible)    \ rtsl  ;.db $DD,$1F,$E9,$3F,$00,$C0
        vctrl(-33d,-50d,visible)    \ rtsl  ;.db $CE,$1F,$DF,$3F,$00,$C0
        vctrl(-47d,-70d,visible)    \ rtsl  ;.db $BA,$1F,$D1,$3F,$00,$C0
        vctrl(-66d,-99d,visible)    \ rtsl  ;.db $9D,$1F,$BE,$3F,$00,$C0
        vctrl(-94d,-141d,visible)   \ rtsl  ;.db $73,$1F,$A2,$3F,$00,$C0
        vctrl(-133d,-199d,visible)  \ rtsl  ;.db $39,$1F,$7B,$3F,$00,$C0      
        
webln01 vctrl(-3d,-6d,visible)      \ rtsl ;.db $FA,$1F,$FD,$3F,$00,$C0
        vctrl(-5d,-9d,visible)      \ rtsl ;.db $F7,$1F,$FB,$3F,$00,$C0
        vctrl(-7d,-12d,visible)     \ rtsl ;.db $F4,$1F,$F9,$3F,$00,$C0
        vctrl(-9d,-18d,visible)     \ rtsl ;.db $EE,$1F,$F7,$3F,$00,$C0
        vctrl(-13d,-25d,visible)    \ rtsl ;.db $E7,$1F,$F3,$3F,$00,$C0
        vctrl(-19d,-35d,visible)    \ rtsl ;.db $DD,$1F,$ED,$3F,$00,$C0
        vctrl(-27d,-50d,visible)    \ rtsl ;.db $CE,$1F,$E5,$3F,$00,$C0
        vctrl(-37d,-70d,visible)    \ rtsl ;.db $BA,$1F,$DB,$3F,$00,$C0
        vctrl(-53d,-99d,visible)    \ rtsl ;.db $9D,$1F,$CB,$3F,$00,$C0
        vctrl(-75d,-141d,visible)   \ rtsl ;.db $73,$1F,$B5,$3F,$00,$C0
        vctrl(-106d,-199d,visible)  \ rtsl ;.db $39,$1F,$96,$3F,$00,$C0
        
webln02 vctrl(-2d,-6d,visible)      \ rtsl ;.db $FA,$1F,$FE,$3F,$00,$C0
        vctrl(-4d,-9d,visible)      \ rtsl ;.db $F7,$1F,$FC,$3F,$00,$C0
        vctrl(-5d,-12d,visible)     \ rtsl ;.db $F4,$1F,$FB,$3F,$00,$C0
        vctrl(-7d,-18d,visible)     \ rtsl ;.db $EE,$1F,$F9,$3F,$00,$C0
        vctrl(-10d,-25d,visible)    \ rtsl ;.db $E7,$1F,$F6,$3F,$00,$C0
        vctrl(-14d,-35d,visible)    \ rtsl ;.db $DD,$1F,$F2,$3F,$00,$C0
        vctrl(-20d,-50d,visible)    \ rtsl ;.db $CE,$1F,$EC,$3F,$00,$C0
        vctrl(-28d,-70d,visible)    \ rtsl ;.db $BA,$1F,$E4,$3F,$00,$C0
        vctrl(-40d,-99d,visible)    \ rtsl ;.db $9D,$1F,$D8,$3F,$00,$C0
        vctrl(-56d,-141d,visible)   \ rtsl ;.db $73,$1F,$C8,$3F,$00,$C0
        vctrl(-80d,-199d,visible)   \ rtsl ;.db $39,$1F,$B0,$3F,$00,$C0
        
webln03 vctrl(-2d,-6d,visible)      \ rtsl ;.db $FA,$1F,$FE,$3F,$00,$C0
        vctrl(-2d,-9d,visible)      \ rtsl ;.db $F7,$1F,$FE,$3F,$00,$C0
        vctrl(-3d,-12d,visible)     \ rtsl ;.db $F4,$1F,$FD,$3F,$00,$C0
        vctrl(-5d,-18d,visible)     \ rtsl ;.db $EE,$1F,$FB,$3F,$00,$C0
        vctrl(-7d,-25d,visible)     \ rtsl ;.db $E7,$1F,$F9,$3F,$00,$C0
        vctrl(-9d,-35d,visible)     \ rtsl ;.db $DD,$1F,$F7,$3F,$00,$C0
        vctrl(-13d,-50d,visible)    \ rtsl ;.db $CE,$1F,$F3,$3F,$00,$C0
        vctrl(-19d,-70d,visible)    \ rtsl ;.db $BA,$1F,$ED,$3F,$00,$C0
        vctrl(-27d,-99d,visible)    \ rtsl ;.db $9D,$1F,$E5,$3F,$00,$C0
        vctrl(-37d,-141d,visible)   \ rtsl ;.db $73,$1F,$DB,$3F,$00,$C0
        vctrl(-53d,-199d,visible)   \ rtsl ;.db $39,$1F,$CB,$3F,$00,$C0
        
webln04 vctrl(-1d,-6d,visible)      \ rtsl ;.db $FA,$1F,$FF,$3F,$00,$C0
        vctrl(-1d,-9d,visible)      \ rtsl ;.db $F7,$1F,$FF,$3F,$00,$C0
        vctrl(-2d,-12d,visible)     \ rtsl ;.db $F4,$1F,$FE,$3F,$00,$C0
        vctrl(-2d,-18d,visible)     \ rtsl ;.db $EE,$1F,$FE,$3F,$00,$C0
        vctrl(-3d,-25d,visible)     \ rtsl ;.db $E7,$1F,$FD,$3F,$00,$C0
        vctrl(-5d,-35d,visible)     \ rtsl ;.db $DD,$1F,$FB,$3F,$00,$C0
        vctrl(-7d,-50d,visible)     \ rtsl ;.db $CE,$1F,$F9,$3F,$00,$C0
        vctrl(-9d,-70d,visible)     \ rtsl ;.db $BA,$1F,$F7,$3F,$00,$C0
        vctrl(-13d,-99d,visible)    \ rtsl ;.db $9D,$1F,$F3,$3F,$00,$C0
        vctrl(-19d,-141d,visible)   \ rtsl ;.db $73,$1F,$ED,$3F,$00,$C0
        vctrl(-27d,-199d,visible)   \ rtsl ;.db $39,$1F,$E5,$3F,$00,$C0
        
        
webln05 vctrl(0d,-6d,visible)       \ rtsl ;.db $FA,$1F,$00,$20,$00,$C0
        vctrl(0d,-9d,visible)       \ rtsl ;.db $F7,$1F,$00,$20,$00,$C0
        vctrl(0d,-12d,visible)      \ rtsl ;.db $F4,$1F,$00,$20,$00,$C0
        vctrl(0d,-18d,visible)      \ rtsl ;.db $EE,$1F,$00,$20,$00,$C0
        vctrl(0d,-25d,visible)      \ rtsl ;.db $E7,$1F,$00,$20,$00,$C0
        vctrl(0d,-35d,visible)      \ rtsl ;.db $DD,$1F,$00,$20,$00,$C0
        vctrl(0d,-50d,visible)      \ rtsl ;.db $CE,$1F,$00,$20,$00,$C0
        vctrl(0d,-70d,visible)      \ rtsl ;.db $BA,$1F,$00,$20,$00,$C0
        vctrl(0d,-99d,visible)      \ rtsl ;.db $9D,$1F,$00,$20,$00,$C0
        vctrl(0d,-141d,visible)     \ rtsl ;.db $73,$1F,$00,$20,$00,$C0
        vctrl(0d,-199d,visible)     \ rtsl ;.db $39,$1F,$00,$20,$00,$C0
        

;***************************************************
;* Linear Line Growth - 1st Quad
;***************************************************            
webln06 vctrl(4d,6d,visible)        \ rtsl ;.db $06,$00,$04,$20,$00,$C0
        vctrl(6d,9d,visible)        \ rtsl ;.db $09,$00,$06,$20,$00,$C0
        vctrl(8d,12d,visible)       \ rtsl ;.db $0C,$00,$08,$20,$00,$C0
        vctrl(12d,18d,visible)      \ rtsl ;.db $12,$00,$0C,$20,$00,$C0
        vctrl(17d,25d,visible)      \ rtsl ;.db $19,$00,$11,$20,$00,$C0
        vctrl(23d,35d,visible)      \ rtsl ;.db $23,$00,$17,$20,$00,$C0
        vctrl(33d,50d,visible)      \ rtsl ;.db $32,$00,$21,$20,$00,$C0
        vctrl(47d,70d,visible)      \ rtsl ;.db $46,$00,$2F,$20,$00,$C0
        vctrl(66d,99d,visible)      \ rtsl ;.db $63,$00,$42,$20,$00,$C0
        vctrl(94d,141d,visible)     \ rtsl ;.db $8D,$00,$5E,$20,$00,$C0
        vctrl(133d,199d,visible)    \ rtsl ;.db $C7,$00,$85,$20,$00,$C0
        
webln07 vctrl(3d,6d,visible)        \ rtsl ;.db $06,$00,$03,$20,$00,$C0
        vctrl(5d,9d,visible)        \ rtsl ;.db $09,$00,$05,$20,$00,$C0
        vctrl(7d,12d,visible)       \ rtsl ;.db $0C,$00,$07,$20,$00,$C0
        vctrl(9d,18d,visible)       \ rtsl ;.db $12,$00,$09,$20,$00,$C0
        vctrl(13d,25d,visible)      \ rtsl ;.db $19,$00,$0D,$20,$00,$C0
        vctrl(19d,35d,visible)      \ rtsl ;.db $23,$00,$13,$20,$00,$C0
        vctrl(27d,50d,visible)      \ rtsl ;.db $32,$00,$1B,$20,$00,$C0
        vctrl(37d,70d,visible)      \ rtsl ;.db $46,$00,$25,$20,$00,$C0
        vctrl(53d,99d,visible)      \ rtsl ;.db $63,$00,$35,$20,$00,$C0
        vctrl(75d,141d,visible)     \ rtsl ;.db $8D,$00,$4B,$20,$00,$C0
        vctrl(106d,199d,visible)    \ rtsl ;.db $C7,$00,$6A,$20,$00,$C0
        
webln08 vctrl(2d,6d,visible)        \ rtsl ;.db $06,$00,$02,$20,$00,$C0
        vctrl(4d,9d,visible)        \ rtsl ;.db $09,$00,$04,$20,$00,$C0
        vctrl(5d,12d,visible)       \ rtsl ;.db $0C,$00,$05,$20,$00,$C0
        vctrl(7d,18d,visible)       \ rtsl ;.db $12,$00,$07,$20,$00,$C0
        vctrl(10d,25d,visible)      \ rtsl ;.db $19,$00,$0A,$20,$00,$C0
        vctrl(14d,35d,visible)      \ rtsl ;.db $23,$00,$0E,$20,$00,$C0
        vctrl(20d,50d,visible)      \ rtsl ;.db $32,$00,$14,$20,$00,$C0
        vctrl(28d,70d,visible)      \ rtsl ;.db $46,$00,$1C,$20,$00,$C0
        vctrl(40d,99d,visible)      \ rtsl ;.db $63,$00,$28,$20,$00,$C0
        vctrl(56d,141d,visible)     \ rtsl ;.db $8D,$00,$38,$20,$00,$C0
        vctrl(80d,199d,visible)     \ rtsl ;.db $C7,$00,$50,$20,$00,$C0
        
webln09 vctrl(2d,6d,visible)        \ rtsl ;.db $06,$00,$02,$20,$00,$C0
        vctrl(2d,9d,visible)        \ rtsl ;.db $09,$00,$02,$20,$00,$C0
        vctrl(3d,12d,visible)       \ rtsl ;.db $0C,$00,$03,$20,$00,$C0
        vctrl(5d,18d,visible)       \ rtsl ;.db $12,$00,$05,$20,$00,$C0
        vctrl(7d,25d,visible)       \ rtsl ;.db $19,$00,$07,$20,$00,$C0
        vctrl(9d,35d,visible)       \ rtsl ;.db $23,$00,$09,$20,$00,$C0
        vctrl(13d,50d,visible)      \ rtsl ;.db $32,$00,$0D,$20,$00,$C0
        vctrl(19d,70d,visible)      \ rtsl ;.db $46,$00,$13,$20,$00,$C0
        vctrl(27d,99d,visible)      \ rtsl ;.db $63,$00,$1B,$20,$00,$C0
        vctrl(37d,141d,visible)     \ rtsl ;.db $8D,$00,$25,$20,$00,$C0
        vctrl(53d,199d,visible)     \ rtsl ;.db $C7,$00,$35,$20,$00,$C0
        
webln0a vctrl(1d,6d,visible)        \ rtsl ;.db $06,$00,$01,$20,$00,$C0
        vctrl(1d,9d,visible)        \ rtsl ;.db $09,$00,$01,$20,$00,$C0
        vctrl(2d,12d,visible)       \ rtsl ;.db $0C,$00,$02,$20,$00,$C0
        vctrl(2d,18d,visible)       \ rtsl ;.db $12,$00,$02,$20,$00,$C0
        vctrl(3d,25d,visible)       \ rtsl ;.db $19,$00,$03,$20,$00,$C0
        vctrl(5d,35d,visible)       \ rtsl ;.db $23,$00,$05,$20,$00,$C0
        vctrl(7d,50d,visible)       \ rtsl ;.db $32,$00,$07,$20,$00,$C0
        vctrl(9d,70d,visible)       \ rtsl ;.db $46,$00,$09,$20,$00,$C0
        vctrl(13d,99d,visible)      \ rtsl ;.db $63,$00,$0D,$20,$00,$C0
        vctrl(19d,141d,visible)     \ rtsl ;.db $8D,$00,$13,$20,$00,$C0
        vctrl(27d,199d,visible)     \ rtsl ;.db $C7,$00,$1B,$20,$00,$C0
        
webln0b vctrl(0d,6d,visible)        \ rtsl ;.db $06,$00,$00,$20,$00,$C0
        vctrl(0d,9d,visible)        \ rtsl ;.db $09,$00,$00,$20,$00,$C0
        vctrl(0d,12d,visible)       \ rtsl ;.db $0C,$00,$00,$20,$00,$C0
        vctrl(0d,18d,visible)       \ rtsl ;.db $12,$00,$00,$20,$00,$C0
        vctrl(0d,25d,visible)       \ rtsl ;.db $19,$00,$00,$20,$00,$C0
        vctrl(0d,35d,visible)       \ rtsl ;.db $23,$00,$00,$20,$00,$C0
        vctrl(0d,50d,visible)       \ rtsl ;.db $32,$00,$00,$20,$00,$C0
        vctrl(0d,70d,visible)       \ rtsl ;.db $46,$00,$00,$20,$00,$C0
        vctrl(0d,99d,visible)       \ rtsl ;.db $63,$00,$00,$20,$00,$C0
        vctrl(0d,141d,visible)      \ rtsl ;.db $8D,$00,$00,$20,$00,$C0
        vctrl(0d,199d,visible)      \ rtsl ;.db $C7,$00,$00,$20,$00,$C0

;***************************************************
;* Linear Line Growth - Horizontal Left
;***************************************************            
weblnh0 vctrl(-2d,0d,visible)   \ rtsl  ;.db $00,$00,$FE,$3F,$00,$C0
weblnh1 vctrl(-3d,0d,visible)   \ rtsl  ;.db $00,$00,$FD,$3F,$00,$C0
weblnh2 vctrl(-4d,0d,visible)   \ rtsl  ;.db $00,$00,$FC,$3F,$00,$C0
weblnh3 vctrl(-6d,0d,visible)   \ rtsl  ;.db $00,$00,$FA,$3F,$00,$C0
weblnh4 vctrl(-8d,0d,visible)   \ rtsl  ;.db $00,$00,$F8,$3F,$00,$C0
weblnh5 vctrl(-11d,0d,visible)  \ rtsl  ;.db $00,$00,$F5,$3F,$00,$C0
weblnh6 vctrl(-16d,0d,visible)  \ rtsl  ;.db $00,$00,$F0,$3F,$00,$C0
weblnh7 vctrl(-23d,0d,visible)  \ rtsl  ;.db $00,$00,$E9,$3F,$00,$C0
weblnh8 vctrl(-32d,0d,visible)  \ rtsl  ;.db $00,$00,$E0,$3F,$00,$C0
weblnh9 vctrl(-45d,0d,visible)  \ rtsl  ;.db $00,$00,$D3,$3F,$00,$C0
weblnha vctrl(-64d,0d,visible)  \ rtsl  ;.db $00,$00,$C0,$3F,$00,$C0
weblnhb vctrl(-91d,0d,visible)  \ rtsl  ;.db $00,$00,$A5,$3F,$00,$C0
        
        
;**************************
;* Vertical Line Folds
;**************************
        
lvert00 vctr(66d,99d,visible)
        rtsl
        ;.db $63,$00,$42,$20,$00,$C0
        
lvert01 vctr(62d,105d,visible)
        vctr(-10d,-6d,hidden)
        vctr(-52d,-97d,visible)
        vctr(73d,97d,visible)
        vctr(-4d,-12d,hidden)
        vctr(-69d,-85d,visible)
        vctr(56d,85d,visible)
        rtsl
        ;.db $69,$00,$3E,$20,$1B,$5D,$9F,$1F,$CC,$3F,$61,$00,$49,$20,$1E,$5A
        ;.db $AB,$1F,$BB,$3F,$55,$00,$38,$20,$00,$C0
        
lvert02 vctr(54d,102d,visible)
        vctr(-20d,-14d,hidden)
        vctr(-33d,-87d,visible)
        vctr(74d,87d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-66d,-63d,visible)
        vctr(41d,63d,visible)
        rtsl
        ;.db $66,$00,$36,$20,$16,$59,$A9,$1F,$DF,$3F,$57,$00,$4A,$20,$1D,$55
        ;.db $C1,$1F,$BE,$3F,$3F,$00,$29,$20,$00,$C0
        
lvert03 vctr(38d,89d,visible)
        vctr(-28d,-20d,hidden)
        vctr(-9d,-68d,visible)
        vctr(68d,68d,visible)
        vctr(-11d,-35d,hidden)
        vctr(-57d,-32d,visible)
        vctr(20d,32d,visible)
        rtsl
        ;.db $59,$00,$26,$20,$12,$56,$BC,$1F,$F7,$3F,$44,$00,$44,$20,$DD,$1F
        ;.db $F5,$1F,$E0,$1F,$C7,$3F,$20,$00,$14,$20,$00,$C0
        
lvert04 vctr(0d,45d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,-13d,visible)
        vctr(43d,13d,visible)
        vctr(-16d,-50d,hidden)
        vctr(-26d,36d,visible)
        vctr(-26d,-36d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$E1,$1F,$D5,$1F,$F3,$1F,$2B,$20,$0D,$00,$2B,$20
        ;.db $CE,$1F,$F0,$1F,$24,$00,$E6,$3F,$DC,$1F,$E6,$3F,$00,$C0
        
lvert05 vctr(-47d,-24d,visible)
        vctr(-43d,-31d,hidden)
        vctr(90d,56d,visible)
        vctr(-3d,-56d,visible)
        vctr(-16d,-51d,hidden)
        vctr(20d,107d,visible)
        vctr(-73d,-107d,visible)
        rtsl
        ;.db $E8,$1F,$D1,$3F,$E1,$1F,$D5,$1F,$38,$00,$5A,$20,$C8,$1F,$FD,$3F
        ;.db $CD,$1F,$F0,$1F,$6B,$00,$14,$20,$95,$1F,$B7,$3F,$00,$C0
        
lvert06 vctr(-71d,-69d,visible)
        vctr(-34d,-25d,hidden)
        vctr(105d,95d,visible)
        vctr(-36d,-95d,visible)
        vctr(-13d,-40d,hidden)
        vctr(49d,136d,visible)
        vctr(-92d,-136d,visible)
        rtsl
        ;.db $BB,$1F,$B9,$3F,$E7,$1F,$DE,$1F,$5F,$00,$69,$20,$A1,$1F,$DC,$3F
        ;.db $D8,$1F,$F3,$1F,$88,$00,$31,$20,$78,$1F,$A4,$3F,$00,$C0
        
lvert07 vctr(-87d,-109d,visible)
        vctr(-20d,-14d,hidden)
        vctr(107d,124d,visible)
        vctr(-66d,-124d,visible)
        vctr(-6d,-22d,hidden)
        vctr(74d,148d,visible)
        vctr(-99d,-148d,visible)
        rtsl
        ;.db $93,$1F,$A9,$3F,$16,$59,$7C,$00,$6B,$20,$84,$1F,$BE,$3F,$1D,$55
        ;.db $94,$00,$4A,$20,$6C,$1F,$9D,$3F,$00,$C0
        
lvert08 vctr(-93d,-140d,visible)
        rtsl
        ;.db $74,$1F,$A3,$3F,$00,$C0
        
        
lvert10 vctr(53d,99d,visible)
        rtsl
        ;.db $63,$00,$35,$20,$00,$C0
        
lvert11 vctr(50d,105d,visible)
        vctr(-10d,-6d,hidden)
        vctr(-39d,-97d,visible)
        vctr(60d,97d,visible)
        vctr(-4d,-12d,hidden)
        vctr(-56d,-85d,visible)
        vctr(43d,85d,visible)
        rtsl
        ;.db $69,$00,$32,$20,$1B,$5D,$9F,$1F,$D9,$3F,$61,$00,$3C,$20,$1E,$5A
        ;.db $AB,$1F,$C8,$3F,$55,$00,$2B,$20,$00,$C0
        
lvert12 vctr(43d,102d,visible)
        vctr(-20d,-14d,hidden)
        vctr(-23d,-87d,visible)
        vctr(63d,87d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-55d,-63d,visible)
        vctr(30d,63d,visible)
        rtsl
        ;.db $66,$00,$2B,$20,$16,$59,$A9,$1F,$E9,$3F,$57,$00,$3F,$20,$1D,$55
        ;.db $C1,$1F,$C9,$3F,$3F,$00,$1E,$20,$00,$C0
        
lvert13 vctr(31d,89d,visible)
        vctr(-28d,-20d,hidden)
        vctr(-1d,-68d,visible)
        vctr(61d,68d,visible)
        vctr(-11d,-35d,hidden)
        vctr(-49d,-32d,visible)
        vctr(12d,32d,visible)
        rtsl
        ;.db $59,$00,$1F,$20,$12,$56,$BC,$1F,$FF,$3F,$44,$00,$3D,$20,$DD,$1F
        ;.db $F5,$1F,$E0,$1F,$CF,$3F,$20,$00,$0C,$20,$00,$C0
        
lvert14 vctr(0d,45d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,-13d,visible)
        vctr(43d,13d,visible)
        vctr(-16d,-50d,hidden)
        vctr(-26d,36d,visible)
        vctr(-26d,-36d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$E1,$1F,$D5,$1F,$F3,$1F,$2B,$20,$0D,$00,$2B,$20
        ;.db $CE,$1F,$F0,$1F,$24,$00,$E6,$3F,$DC,$1F,$E6,$3F,$00,$C0
        
lvert15 vctr(-37d,-24d,visible)
        vctr(-43d,-31d,hidden)
        vctr(81d,56d,visible)
        vctr(5d,-56d,visible)
        vctr(-16d,-51d,hidden)
        vctr(10d,107d,visible)
        vctr(-64d,-107d,visible)
        rtsl
        ;.db $E8,$1F,$DB,$3F,$E1,$1F,$D5,$1F,$38,$00,$51,$20,$C8,$1F,$05,$20
        ;.db $CD,$1F,$F0,$1F,$6B,$00,$0A,$20,$95,$1F,$C0,$3F,$00,$C0
        
lvert16 vctr(-56d,-69d,visible)
        vctr(-34d,-25d,hidden)
        vctr(91d,95d,visible)
        vctr(-22d,-95d,visible)
        vctr(-13d,-40d,hidden)
        vctr(35d,136d,visible)
        vctr(-78d,-136d,visible)
        rtsl
        ;.db $BB,$1F,$C8,$3F,$E7,$1F,$DE,$1F,$5F,$00,$5B,$20,$A1,$1F,$EA,$3F
        ;.db $D8,$1F,$F3,$1F,$88,$00,$23,$20,$78,$1F,$B2,$3F,$00,$C0
        
lvert17 vctr(-69d,-109d,visible)
        vctr(-20d,-14d,hidden)
        vctr(90d,124d,visible)
        vctr(-49d,-124d,visible)
        vctr(-6d,-22d,hidden)
        vctr(57d,148d,visible)
        vctr(-82d,-148d,visible)
        rtsl
        ;.db $93,$1F,$BB,$3F,$16,$59,$7C,$00,$5A,$20,$84,$1F,$CF,$3F,$1D,$55
        ;.db $94,$00,$39,$20,$6C,$1F,$AE,$3F,$00,$C0
        
lvert18 vctr(-74d,-140d,visible)
        rtsl
        ;.db $74,$1F,$B6,$3F,$00,$C0
        
        
lvert20 vctr(39d,99d,visible)
        rtsl
        ;.db $63,$00,$27,$20,$00,$C0
        
lvert21 vctr(37d,105d,visible)
        vctr(-10d,-6d,hidden)
        vctr(-27d,-97d,visible)
        vctr(48d,97d,visible)
        vctr(-4d,-12d,hidden)
        vctr(-44d,-85d,visible)
        vctr(31d,85d,visible)
        rtsl
        ;.db $69,$00,$25,$20,$1B,$5D,$9F,$1F,$E5,$3F,$61,$00,$30,$20,$1E,$5A
        ;.db $AB,$1F,$D4,$3F,$55,$00,$1F,$20,$00,$C0
        
lvert22 vctr(32d,102d,visible)
        vctr(-20d,-14d,hidden)
        vctr(-12d,-87d,visible)
        vctr(52d,87d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-44d,-63d,visible)
        vctr(19d,63d,visible)
        rtsl    
        ;.db $66,$00,$20,$20,$16,$59,$A9,$1F,$F4,$3F,$57,$00,$34,$20,$1D,$55
        ;.db $C1,$1F,$D4,$3F,$3F,$00,$13,$20,$00,$C0
        
lvert23 vctr(23d,89d,visible)
        vctr(-28d,-20d,hidden)
        vctr(6d,-68d,visible)
        vctr(53d,68d,visible)
        vctr(-11d,-35d,hidden)
        vctr(-41d,-32d,visible)
        vctr(4d,32d,visible)
        rtsl
        ;.db $59,$00,$17,$20,$12,$56,$BC,$1F,$06,$20,$44,$00,$35,$20,$DD,$1F
        ;.db $F5,$1F,$E0,$1F,$D7,$3F,$20,$00,$04,$20,$00,$C0
        
lvert24 vctr(0d,45d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,-13d,visible)
        vctr(43d,13d,visible)
        vctr(-16d,-50d,hidden)
        vctr(-26d,36d,visible)
        vctr(-26d,-36d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$E1,$1F,$D5,$1F,$F3,$1F,$2B,$20,$0D,$00,$2B,$20
        ;.db $CE,$1F,$F0,$1F,$24,$00,$E6,$3F,$DC,$1F,$E6,$3F,$00,$C0
        
lvert25 vctr(-28d,-24d,visible)
        vctr(-43d,-31d,hidden)
        vctr(71d,56d,visible)
        vctr(15d,-56d,visible)
        vctr(-16d,-51d,hidden)
        vctr(1d,107d,visible)
        vctr(-55d,-107d,visible)
        rtsl
        ;.db $32,$54,$E1,$1F,$D5,$1F,$38,$00,$47,$20,$C8,$1F,$0F,$20,$CD,$1F
        ;.db $F0,$1F,$6B,$00,$01,$20,$95,$1F,$C9,$3F,$00,$C0
        
lvert26 vctr(-42d,-69d,visible)
        vctr(-34d,-25d,hidden)
        vctr(77d,95d,visible)
        vctr(-7d,-95d,visible)
        vctr(-13d,-40d,hidden)
        vctr(21d,136d,visible)
        vctr(-64d,-136d,visible)
        rtsl
        ;.db $BB,$1F,$D6,$3F,$E7,$1F,$DE,$1F,$5F,$00,$4D,$20,$A1,$1F,$F9,$3F
        ;.db $D8,$1F,$F3,$1F,$88,$00,$15,$20,$78,$1F,$C0,$3F,$00,$C0
        
lvert27 vctr(-52d,-109d,visible)
        vctr(-20d,-14d,hidden)
        vctr(72d,124d,visible)
        vctr(-31d,-124d,visible)
        vctr(-6d,-22d,hidden)
        vctr(39d,148d,visible)
        vctr(-64d,-148d,visible)
        rtsl
        ;.db $93,$1F,$CC,$3F,$16,$59,$7C,$00,$48,$20,$84,$1F,$E1,$3F,$1D,$55
        ;.db $94,$00,$27,$20,$6C,$1F,$C0,$3F,$00,$C0
        
lvert28 vctr(-56d,-140d,visible)
        rtsl
        ;.db $74,$1F,$C8,$3F,$00,$C0
        
        
lvert30 vctr(26d,99d,visible)
        rtsl
        ;.db $63,$00,$1A,$20,$00,$C0
        
lvert31 vctr(25d,105d,visible)
        vctr(-10d,-6d,hidden)
        vctr(-14d,-97d,visible)
        vctr(35d,97d,visible)
        vctr(-4d,-12d,hidden)
        vctr(-31d,-85d,visible)
        vctr(18d,85d,visible)
        rtsl
        ;.db $69,$00,$19,$20,$1B,$5D,$9F,$1F,$F2,$3F,$61,$00,$23,$20,$1E,$5A
        ;.db $AB,$1F,$E1,$3F,$55,$00,$12,$20,$00,$C0
        
lvert32 vctr(21d,102d,visible)
        vctr(-20d,-14d,hidden)
        vctr(-1d,-87d,visible)
        vctr(41d,87d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-34d,-63d,visible)
        vctr(9d,63d,visible)
        rtsl
        ;.db $66,$00,$15,$20,$16,$59,$A9,$1F,$FF,$3F,$57,$00,$29,$20,$1D,$55
        ;.db $C1,$1F,$DE,$3F,$3F,$00,$09,$20,$00,$C0
        
lvert33 vctr(15d,89d,visible)
        vctr(-28d,-20d,hidden)
        vctr(14d,-68d,visible)
        vctr(45d,68d,visible)
        vctr(-11d,-35d,hidden)
        vctr(-34d,-32d,visible)
        vctr(-2d,32d,visible)
        rtsl
        ;.db $59,$00,$0F,$20,$12,$56,$BC,$1F,$0E,$20,$44,$00,$2D,$20,$DD,$1F
        ;.db $F5,$1F,$E0,$1F,$DE,$3F,$20,$00,$FE,$3F,$00,$C0
        
lvert34 vctr(0d,45d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,-13d,visible)
        vctr(43d,13d,visible)
        vctr(-16d,-50d,hidden)
        vctr(-26d,36d,visible)
        vctr(-26d,-36d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$E1,$1F,$D5,$1F,$F3,$1F,$2B,$20,$0D,$00,$2B,$20
        ;.db $CE,$1F,$F0,$1F,$24,$00,$E6,$3F,$DC,$1F,$E6,$3F,$00,$C0
        
lvert35 vctr(-18d,-24d,visible)
        vctr(-43d,-31d,hidden)
        vctr(62d,56d,visible)
        vctr(24d,-56d,visible)
        vctr(-16d,-51d,hidden)
        vctr(-8d,107d,visible)
        vctr(-45d,-107d,visible)
        rtsl
        ;.db $37,$54,$E1,$1F,$D5,$1F,$38,$00,$3E,$20,$C8,$1F,$18,$20,$CD,$1F
        ;.db $F0,$1F,$6B,$00,$F8,$3F,$95,$1F,$D3,$3F,$00,$C0
        
lvert36 vctr(-28d,-69d,visible)
        vctr(-34d,-25d,hidden)
        vctr(63d,95d,visible)
        vctr(6d,-95d,visible)
        vctr(-13d,-40d,hidden)
        vctr(6d,136d,visible)
        vctr(-49d,-136d,visible)
        rtsl
        ;.db $BB,$1F,$E4,$3F,$E7,$1F,$DE,$1F,$5F,$00,$3F,$20,$A1,$1F,$06,$20
        ;.db $D8,$1F,$F3,$1F,$88,$00,$06,$20,$78,$1F,$CF,$3F,$00,$C0
        
lvert37 vctr(-34d,-109d,visible)
        vctr(-20d,-14d,hidden)
        vctr(55d,124d,visible)
        vctr(-14d,-124d,visible)
        vctr(-6d,-22d,hidden)
        vctr(22d,148d,visible)
        vctr(-47d,-148d,visible)
        rtsl
        ;.db $93,$1F,$DE,$3F,$16,$59,$7C,$00,$37,$20,$84,$1F,$F2,$3F,$1D,$55
        ;.db $94,$00,$16,$20,$6C,$1F,$D1,$3F,$00,$C0
        
lvert38 vctr(-37d,-140d,visible)
        rtsl
        ;.db $74,$1F,$DB,$3F,$00,$C0
        
lvert40 vctr(13d,99d,visible)
        rtsl
        ;.db $63,$00,$0D,$20,$00,$C0
        
lvert41 vctr(12d,105d,visible)
        vctr(-10d,-6d,hidden)
        vctr(-1d,-97d,visible)
        vctr(23d,97d,visible)
        vctr(-4d,-12d,hidden)
        vctr(-19d,-85d,visible)
        vctr(6d,85d,visible)
        rtsl
        ;.db $69,$00,$0C,$20,$1B,$5D,$9F,$1F,$FF,$3F,$61,$00,$17,$20,$1E,$5A
        ;.db $AB,$1F,$ED,$3F,$55,$00,$06,$20,$00,$C0
        
lvert42 vctr(10d,102d,visible)
        vctr(-20d,-14d,hidden)
        vctr(9d,-87d,visible)
        vctr(30d,87d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-23d,-63d,visible)
        vctr(-1d,63d,visible)
        rtsl
        ;.db $66,$00,$0A,$20,$16,$59,$A9,$1F,$09,$20,$57,$00,$1E,$20,$1D,$55
        ;.db $C1,$1F,$E9,$3F,$3F,$00,$FF,$3F,$00,$C0
        
lvert43 vctr(7d,89d,visible)
        vctr(-28d,-20d,hidden)
        vctr(22d,-68d,visible)
        vctr(37d,68d,visible)
        vctr(-11d,-35d,hidden)
        vctr(-26d,-32d,visible)
        vctr(-10d,32d,visible)
        rtsl
        ;.db $59,$00,$07,$20,$12,$56,$BC,$1F,$16,$20,$44,$00,$25,$20,$DD,$1F
        ;.db $F5,$1F,$E0,$1F,$E6,$3F,$20,$00,$F6,$3F,$00,$C0
        
lvert44 vctr(0d,45d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,-13d,visible)
        vctr(43d,13d,visible)
        vctr(-16d,-50d,hidden)
        vctr(-26d,36d,visible)
        vctr(-26d,-36d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$E1,$1F,$D5,$1F,$F3,$1F,$2B,$20,$0D,$00,$2B,$20
        ;.db $CE,$1F,$F0,$1F,$24,$00,$E6,$3F,$DC,$1F,$E6,$3F,$00,$C0

lvert45 vctr(-8d,-24d,visible)
        vctr(-43d,-31d,hidden)
        vctr(52d,56d,visible)
        vctr(34d,-56d,visible)
        vctr(-16d,-51d,hidden)
        vctr(-17d,107d,visible)
        vctr(-36d,-107d,visible)
        rtsl
        ;.db $3C,$54,$E1,$1F,$D5,$1F,$38,$00,$34,$20,$C8,$1F,$22,$20,$CD,$1F
        ;.db $F0,$1F,$6B,$00,$EF,$3F,$95,$1F,$DC,$3F,$00,$C0
        
lvert46 vctr(-14d,-69d,visible)
        vctr(-34d,-25d,hidden)
        vctr(49d,95d,visible)
        vctr(20d,-95d,visible)
        vctr(-13d,-40d,hidden)
        vctr(-7d,136d,visible)
        vctr(-35d,-136d,visible)
        rtsl
        ;.db $BB,$1F,$F2,$3F,$E7,$1F,$DE,$1F,$5F,$00,$31,$20,$A1,$1F,$14,$20
        ;.db $D8,$1F,$F3,$1F,$88,$00,$F9,$3F,$78,$1F,$DD,$3F,$00,$C0
        
lvert47 vctr(-17d,-109d,visible)
        vctr(-20d,-14d,hidden)
        vctr(37d,124d,visible)
        vctr(2d,-124d,visible)
        vctr(-6d,-22d,hidden)
        vctr(4d,148d,visible)
        vctr(-30d,-148d,visible)
        rtsl
        ;.db $93,$1F,$EF,$3F,$16,$59,$7C,$00,$25,$20,$84,$1F,$02,$20,$1D,$55
        ;.db $94,$00,$04,$20,$6C,$1F,$E2,$3F,$00,$C0
        
lvert48 vctr(-18d,-140d,visible)
        rtsl
        ;.db $74,$1F,$EE,$3F,$00,$C0
        
lvert50 vctr(0d,99d,visible)
        rtsl
        ;.db $63,$00,$00,$20,$00,$C0
        
lvert51 vctr(0d,105d,visible)
        vctr(-10d,-6d,hidden)
        vctr(10d,-97d,visible)
        vctr(10d,97d,visible)
        vctr(-4d,-12d,hidden)
        vctr(-6d,-85d,visible)
        vctr(-6d,85d,visible)
        rtsl
        ;.db $69,$00,$00,$20,$1B,$5D,$9F,$1F,$0A,$20,$61,$00,$0A,$20,$1E,$5A
        ;.db $AB,$1F,$FA,$3F,$55,$00,$FA,$3F,$00,$C0
        
lvert52 vctr(0d,102d,visible)
        vctr(-20d,-14d,hidden)
        vctr(20d,-87d,visible)
        vctr(20d,87d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-12d,-63d,visible)
        vctr(-12d,63d,visible)
        rtsl
        ;.db $66,$00,$00,$20,$16,$59,$A9,$1F,$14,$20,$57,$00,$14,$20,$1D,$55
        ;.db $C1,$1F,$F4,$3F,$3F,$00,$F4,$3F,$00,$C0
        
lvert53 vctr(0d,89d,visible)
        vctr(-28d,-20d,hidden)
        vctr(29d,-68d,visible)
        vctr(29d,68d,visible)
        vctr(-11d,-35d,hidden)
        vctr(-18d,-32d,visible)
        vctr(-18d,32d,visible)
        rtsl
        ;.db $59,$00,$00,$20,$12,$56,$BC,$1F,$1D,$20,$44,$00,$1D,$20,$DD,$1F
        ;.db $F5,$1F,$E0,$1F,$EE,$3F,$20,$00,$EE,$3F,$00,$C0
        
lvert54 vctr(0d,45d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,-13d,visible)
        vctr(43d,13d,visible)
        vctr(-16d,-50d,hidden)
        vctr(-26d,36d,visible)
        vctr(-26d,-36d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$E1,$1F,$D5,$1F,$F3,$1F,$2B,$20,$0D,$00,$2B,$20
        ;.db $CE,$1F,$F0,$1F,$24,$00,$E6,$3F,$DC,$1F,$E6,$3F,$00,$C0
        
lvert55 vctr(0d,-24d,visible)
        vctr(-43d,-31d,hidden)
        vctr(43d,56d,visible)
        vctr(43d,-56d,visible)
        vctr(-16d,-51d,hidden)
        vctr(-26d,107d,visible)
        vctr(-26d,-107d,visible)
        rtsl
        ;.db $20,$54,$E1,$1F,$D5,$1F,$38,$00,$2B,$20,$C8,$1F,$2B,$20,$CD,$1F
        ;.db $F0,$1F,$6B,$00,$E6,$3F,$95,$1F,$E6,$3F,$00,$C0
        
lvert56 vctr(0d,-69d,visible)
        vctr(-34d,-25d,hidden)
        vctr(34d,95d,visible)
        vctr(34d,-95d,visible)
        vctr(-13d,-40d,hidden)
        vctr(-21d,136d,visible)
        vctr(-21d,-136d,visible)
        rtsl
        ;.db $BB,$1F,$00,$20,$E7,$1F,$DE,$1F,$5F,$00,$22,$20,$A1,$1F,$22,$20
        ;.db $D8,$1F,$F3,$1F,$88,$00,$EB,$3F,$78,$1F,$EB,$3F,$00,$C0
        
lvert57 vctr(0d,-109d,visible)
        vctr(-20d,-14d,hidden)
        vctr(20d,124d,visible)
        vctr(20d,-124d,visible)
        vctr(-6d,-22d,hidden)
        vctr(-12d,148d,visible)
        vctr(-12d,-148d,visible)
        rtsl
        ;.db $93,$1F,$00,$20,$16,$59,$7C,$00,$14,$20,$84,$1F,$14,$20,$1D,$55
        ;.db $94,$00,$F4,$3F,$6C,$1F,$F4,$3F,$00,$C0
        
lvert58 vctr(0d,-140d,visible)
        rtsl
        ;.db $74,$1F,$00,$20,$00,$C0
;***********************
        
hfold20 vctr(0d,45d,visible)
        vctr(34d,18d,hidden)
        vctr(-34d,-63d,visible)
        vctr(-40d,-44d,visible)
        vctr(16d,-32d,hidden)
        vctr(24d,76d,visible)
        vctr(20d,0d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$12,$00,$22,$00,$C1,$1F,$DE,$3F,$D4,$1F,$D8,$3F
        ;.db $E0,$1F,$10,$00,$4C,$00,$18,$20,$2A,$40,$00,$C0
        
hfold21 vctr(17d,41d,visible)
        vctr(28d,16d,hidden)
        vctr(-46d,-59d,visible)
        vctr(-17d,-40d,visible)
        vctr(12d,-28d,hidden)
        vctr(3d,70d,visible)
        vctr(36d,0d,visible)
        rtsl
        ;.db $29,$00,$11,$20,$0E,$48,$C5,$1F,$D2,$3F,$D8,$1F,$EF,$3F,$06,$52
        ;.db $46,$00,$03,$20,$00,$00,$24,$20,$00,$C0
        
hfold22 vctr(32d,32d,visible)
        vctr(20d,14d,hidden)
        vctr(-53d,-46d,visible)
        vctr(6d,-30d,visible)
        vctr(8d,-22d,hidden)
        vctr(-17d,53d,visible)
        vctr(45d,0d,visible)
        rtsl
        ;.db $20,$00,$20,$20,$0A,$47,$D2,$1F,$CB,$3F,$23,$51,$04,$55,$35,$00
        ;.db $EF,$3F,$00,$00,$2D,$20,$00,$C0
        
hfold23 vctr(41d,17d,visible)
        vctr(10d,8d,hidden)
        vctr(-53d,-25d,visible)
        vctr(28d,-16d,visible)
        vctr(4d,-10d,hidden)
        vctr(-34d,28d,visible)
        vctr(48d,0d,visible)
        rtsl
        ;.db $11,$00,$29,$20,$05,$44,$E7,$1F,$CB,$3F,$2E,$58,$02,$5B,$1C,$00
        ;.db $DE,$3F,$00,$00,$30,$20,$00,$C0
        
hfold24 vctr(45d,0d,visible)
        rtsl
        ;.db $00,$00,$2D,$20,$00,$C0
    
;**************************
;* Horizontal Line Folds
;************************** 
        
lhorz00 vctr(-45d,0d,visible)
        rtsl
        ;.db $00,$00,$D3,$3F,$00,$C0
        
lhorz01 vctr(-41d,17d,visible)
        vctr(12d,8d,hidden)
        vctr(28d,-24d,visible)
        vctr(-56d,-16d,visible)
        vctr(4d,-10d,hidden)
        vctr(50d,28d,visible)
        vctr(-33d,0d,visible)
        rtsl
        ;.db $11,$00,$D7,$3F,$06,$44,$2E,$54,$F0,$1F,$C8,$3F,$02,$5B,$1C,$00
        ;.db $32,$20,$00,$00,$DF,$3F,$00,$C0
        
lhorz02 vctr(-31d,32d,visible)
        vctr(22d,14d,hidden)
        vctr(8d,-46d,visible)
        vctr(-58d,-30d,visible)
        vctr(10d,-22d,hidden)
        vctr(47d,53d,visible)
        vctr(-16d,0d,visible)
        rtsl
        ;.db $20,$00,$E1,$3F,$0B,$47,$D2,$1F,$08,$20,$E2,$1F,$C6,$3F,$05,$55
        ;.db $35,$00,$2F,$20,$38,$40,$00,$C0
        
lhorz03 vctr(-17d,41d,visible)
        vctr(28d,16d,hidden)
        vctr(-10d,-59d,visible)
        vctr(-49d,-40d,visible)
        vctr(12d,-28d,hidden)
        vctr(36d,70d,visible)
        vctr(2d,0d,visible)
        rtsl
        ;.db $29,$00,$EF,$3F,$0E,$48,$C5,$1F,$F6,$3F,$D8,$1F,$CF,$3F,$06,$52
        ;.db $46,$00,$24,$20,$21,$40,$00,$C0
        
lhorz04 vctr(0d,45d,visible)
        vctr(26d,18d,hidden)
        vctr(-27d,-63d,visible)
        vctr(-32d,-44d,visible)
        vctr(13d,-32d,hidden)
        vctr(19d,76d,visible)
        vctr(16d,0d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$0D,$49,$C1,$1F,$E5,$3F,$D4,$1F,$E0,$3F,$E0,$1F
        ;.db $0D,$00,$4C,$00,$13,$20,$28,$40,$00,$C0
        
lhorz05 vctr(17d,41d,visible)
        vctr(22d,16d,hidden)
        vctr(-40d,-59d,visible)
        vctr(-9d,-40d,visible)
        vctr(10d,-28d,hidden)
        vctr(-1d,70d,visible)
        vctr(32d,0d,visible)
        rtsl
        ;.db $29,$00,$11,$20,$0B,$48,$C5,$1F,$D8,$3F,$D8,$1F,$F7,$3F,$05,$52
        ;.db $46,$00,$FF,$3F,$00,$00,$20,$20,$00,$C0
        
lhorz06 vctr(32d,32d,visible)
        vctr(16d,14d,hidden)
        vctr(-48d,-46d,visible)
        vctr(12d,-30d,visible)
        vctr(6d,-22d,hidden)
        vctr(-20d,53d,visible)
        vctr(42d,0d,visible)
        rtsl
        ;.db $20,$00,$20,$20,$08,$47,$D2,$1F,$D0,$3F,$26,$51,$03,$55,$35,$00
        ;.db $EC,$3F,$00,$00,$2A,$20,$00,$C0
        
lhorz07 vctr(41d,17d,visible)
        vctr(8d,8d,hidden)
        vctr(-50d,-25d,visible)
        vctr(32d,-16d,visible)
        vctr(2d,-10d,hidden)
        vctr(-36d,28d,visible)
        vctr(47d,0d,visible)
        rtsl
        ;.db $11,$00,$29,$20,$04,$44,$E7,$1F,$CE,$3F,$F0,$1F,$20,$20,$01,$5B
        ;.db $1C,$00,$DC,$3F,$00,$00,$2F,$20,$00,$C0
        
lhorz08 vctr(45d,0d,visible)
        rtsl
        ;.db $00,$00,$2D,$20,$00,$C0
        
lhorz10 vctr(-45d,0d,visible)
        rtsl
        ;.db $00,$00,$D3,$3F,$00,$C0
        
lhorz11 vctr(-41d,17d,visible)
        vctr(10d,8d,hidden)
        vctr(30d,-24d,visible)
        vctr(-53d,-16d,visible)
        vctr(4d,-10d,hidden)
        vctr(48d,28d,visible)
        vctr(-35d,0d,visible)
        rtsl
        ;.db $11,$00,$D7,$3F,$05,$44,$2F,$54,$F0,$1F,$CB,$3F,$02,$5B,$1C,$00
        ;.db $30,$20,$00,$00,$DD,$3F,$00,$C0
        
lhorz12 vctr(-32d,32d,visible)
        vctr(18d,14d,hidden)
        vctr(13d,-46d,visible)
        vctr(-52d,-30d,visible)
        vctr(8d,-22d,hidden)
        vctr(44d,53d,visible)
        vctr(-20d,0d,visible)
        rtsl
        ;.db $20,$00,$E0,$3F,$09,$47,$D2,$1F,$0D,$20,$E2,$1F,$CC,$3F,$04,$55 
        ;.db $35,$00,$2C,$20,$36,$40,$00,$C0
        
lhorz13 vctr(-17d,41d,visible)
        vctr(20d,16d,hidden)
        vctr(-4d,-59d,visible)
        vctr(-42d,-40d,visible)
        vctr(10d,-28d,hidden)
        vctr(32d,70d,visible)
        vctr(-2d,0d,visible)
        rtsl
        ;.db $29,$00,$EF,$3F,$0A,$48,$C5,$1F,$FC,$3F,$D8,$1F,$D6,$3F,$05,$52
        ;.db $46,$00,$20,$20,$3F,$40,$00,$C0
        
lhorz14 vctr(0d,45d,visible)
        vctr(20d,18d,hidden)
        vctr(-20d,-63d,visible)
        vctr(-24d,-44d,visible)
        vctr(9d,-32d,hidden)
        vctr(14d,76d,visible)
        vctr(12d,0d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$0A,$49,$C1,$1F,$EC,$3F,$D4,$1F,$E8,$3F,$E0,$1F
        ;.db $09,$00,$4C,$00,$0E,$20,$26,$40,$00,$C0
        
lhorz15 vctr(17d,41d,visible)
        vctr(16d,16d,hidden)
        vctr(-34d,-59d,visible)
        vctr(-2d,-40d,visible)
        vctr(6d,-28d,hidden)
        vctr(-5d,70d,visible)
        vctr(26d,0d,visible)
        rtsl
        ;.db $29,$00,$11,$20,$08,$48,$C5,$1F,$DE,$3F,$D8,$1F,$FE,$3F,$03,$52
        ;.db $46,$00,$FB,$3F,$2D,$40,$00,$C0
        
lhorz16 vctr(32d,32d,visible)
        vctr(10d,14d,hidden)
        vctr(-43d,-46d,visible)
        vctr(18d,-30d,visible)
        vctr(4d,-22d,hidden)
        vctr(-24d,53d,visible)
        vctr(39d,0d,visible)
        rtsl
        ;.db $20,$00,$20,$20,$05,$47,$D2,$1F,$D5,$3F,$29,$51,$02,$55,$35,$00
        ;.db $E8,$3F,$00,$00,$27,$20,$00,$C0
        
lhorz17 vctr(41d,17d,visible)
        vctr(4d,8d,hidden)
        vctr(-47d,-25d,visible)
        vctr(35d,-16d,visible)
        vctr(2d,-10d,hidden)
        vctr(-38d,28d,visible)
        vctr(45d,0d,visible)
        rtsl
        ;.db $11,$00,$29,$20,$02,$44,$E7,$1F,$D1,$3F,$F0,$1F,$23,$20,$01,$5B
        ;.db $1C,$00,$DA,$3F,$00,$00,$2D,$20,$00,$C0
        
lhorz18 vctr(45d,0d,visible)
        rtsl
        ;.db $00,$00,$2D,$20,$00,$C0
        
lhorz20 vctr(-45d,0d,visible)
        rtsl
        ;.db $00,$00,$D3,$3F,$00,$C0
        
lhorz21 vctr(-41d,17d,visible)
        vctr(8d,8d,hidden)
        vctr(33d,-25d,visible)
        vctr(-50d,-16d,visible)
        vctr(2d,-10d,hidden)
        vctr(47d,28d,visible)
        vctr(-36d,0d,visible)
        rtsl
        ;.db $11,$00,$D7,$3F,$04,$44,$E7,$1F,$21,$20,$F0,$1F,$CE,$3F,$01,$5B
        ;.db $1C,$00,$2F,$20,$00,$00,$DC,$3F,$00,$C0
        
lhorz22 vctr(-32d,32d,visible)
        vctr(12d,14d,hidden)
        vctr(18d,-46d,visible)
        vctr(-47d,-30d,visible)
        vctr(4d,-22d,hidden)
        vctr(41d,53d,visible)
        vctr(-22d,0d,visible)
        rtsl
        ;.db $20,$00,$E0,$3F,$06,$47,$D2,$1F,$12,$20,$E2,$1F,$D1,$3F,$02,$55
        ;.db $35,$00,$29,$20,$35,$40,$00,$C0
        
lhorz23 vctr(-17d,41d,visible)
        vctr(14d,16d,hidden)
        vctr(2d,-59d,visible)
        vctr(-35d,-40d,visible)
        vctr(6d,-28d,hidden)
        vctr(27d,70d,visible)
        vctr(-6d,0d,visible)
        rtsl
        ;.db $29,$00,$EF,$3F,$07,$48,$C5,$1F,$02,$20,$D8,$1F,$DD,$3F,$03,$52
        ;.db $46,$00,$1B,$20,$3D,$40,$00,$C0
        
lhorz24 vctr(0d,45d,visible)
        vctr(12d,18d,hidden)
        vctr(-13d,-63d,visible)
        vctr(-16d,-44d,visible)
        vctr(6d,-32d,hidden)
        vctr(9d,76d,visible)
        vctr(8d,0d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$06,$49,$C1,$1F,$F3,$3F,$D4,$1F,$F0,$3F,$E0,$1F
        ;.db $06,$00,$4C,$00,$09,$20,$24,$40,$00,$C0
        
lhorz25 vctr(17d,41d,visible)
        vctr(10d,16d,hidden)
        vctr(-27d,-59d,visible)
        vctr(5d,-40d,visible)
        vctr(4d,-28d,hidden)
        vctr(-10d,70d,visible)
        vctr(22d,0d,visible)
        rtsl
        ;.db $29,$00,$11,$20,$05,$48,$C5,$1F,$E5,$3F,$D8,$1F,$05,$20,$02,$52
        ;.db $46,$00,$F6,$3F,$2B,$40,$00,$C0
        
lhorz26 vctr(32d,32d,visible)
        vctr(6d,14d,hidden)
        vctr(-38d,-46d,visible)
        vctr(24d,-30d,visible)
        vctr(2d,-22d,hidden)
        vctr(-27d,53d,visible)
        vctr(36d,0d,visible)
        rtsl
        ;.db $20,$00,$20,$20,$03,$47,$D2,$1F,$DA,$3F,$2C,$51,$01,$55,$35,$00
        ;.db $E5,$3F,$00,$00,$24,$20,$00,$C0
        
lhorz27 vctr(41d,17d,visible)
        vctr(2d,8d,hidden)
        vctr(-44d,-25d,visible)
        vctr(38d,-16d,visible)
        vctr(0d,-10d,hidden)
        vctr(-39d,28d,visible)
        vctr(43d,0d,visible)
        rtsl
        ;.db $11,$00,$29,$20,$01,$44,$E7,$1F,$D4,$3F,$F0,$1F,$26,$20,$00,$5B
        ;.db $1C,$00,$D9,$3F,$00,$00,$2B,$20,$00,$C0
        
lhorz28 vctr(41d,17d,visible)
        vctr(2d,8d,hidden)
        vctr(-44d,-25d,visible)
        vctr(38d,-16d,visible)
        vctr(0d,-10d,hidden)
        vctr(-39d,28d,visible)
        vctr(43d,0d,visible)
        rtsl
        ;.db $00,$00,$2D,$20,$00,$C0
        
lhorz30 vctr(-45d,0d,visible)
        rtsl
        ;.db $00,$00,$D3,$3F,$00,$C0
        
lhorz31 vctr(-41d,17d,visible)
        vctr(4d,8d,hidden)
        vctr(36d,-25d,visible)
        vctr(-47d,-16d,visible)
        vctr(2d,-10d,hidden)
        vctr(45d,28d,visible)
        vctr(-38d,0d,visible)
        rtsl
        ;.db $11,$00,$D7,$3F,$02,$44,$E7,$1F,$24,$20,$F0,$1F,$D1,$3F,$01,$5B
        ;.db $1C,$00,$2D,$20,$00,$00,$DA,$3F,$00,$C0
        
lhorz32 vctr(-32d,32d,visible)
        vctr(8d,14d,hidden)
        vctr(23d,-46d,visible)
        vctr(-41d,-30d,visible)
        vctr(2d,-22d,hidden)
        vctr(37d,53d,visible)
        vctr(-26d,0d,visible)
        rtsl
        ;.db $20,$00,$E0,$3F,$04,$47,$D2,$1F,$17,$20,$E2,$1F,$D7,$3F,$01,$55
        ;.db $35,$00,$25,$20,$33,$40,$00,$C0
        
lhorz33 vctr(-17d,41d,visible)
        vctr(8d,16d,hidden)
        vctr(8d,-59d,visible)
        vctr(-27d,-40d,visible)
        vctr(4d,-28d,hidden)
        vctr(23d,70d,visible)
        vctr(-10d,0d,visible)
        rtsl
        ;.db $29,$00,$EF,$3F,$04,$48,$C5,$1F,$08,$20,$D8,$1F,$E5,$3F,$02,$52
        ;.db $46,$00,$17,$20,$3B,$40,$00,$C0
        
lhorz34 vctr(0d,45d,visible)
        vctr(6d,18d,hidden)
        vctr(-6d,-63d,visible)
        vctr(-8d,-44d,visible)
        vctr(3d,-32d,hidden)
        vctr(4d,76d,visible)
        vctr(4d,0d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$03,$49,$C1,$1F,$FA,$3F,$D4,$1F,$F8,$3F,$E0,$1F
        ;.db $03,$00,$4C,$00,$04,$20,$22,$40,$00,$C0
        
lhorz35 vctr(17d,41d,visible)
        vctr(2d,16d,hidden)
        vctr(-21d,-59d,visible)
        vctr(12d,-40d,visible)
        vctr(0d,-28d,hidden)
        vctr(-14d,70d,visible)
        vctr(18d,0d,visible)
        rtsl
        ;.db $29,$00,$11,$20,$01,$48,$C5,$1F,$EB,$3F,$D8,$1F,$0C,$20,$00,$52
        ;.db $46,$00,$F2,$3F,$29,$40,$00,$C0
        
lhorz36 vctr(31d,32d,visible)
        vctr(0d,14d,hidden)
        vctr(-33d,-46d,visible)
        vctr(30d,-30d,visible)
        vctr(0d,-22d,hidden)
        vctr(-31d,53d,visible)
        vctr(32d,0d,visible)
        rtsl
        ;.db $20,$00,$1F,$20,$00,$47,$D2,$1F,$DF,$3F,$2F,$51,$00,$55,$35,$00
        ;.db $E1,$3F,$00,$00,$20,$20,$00,$C0
        
lhorz37 vctr(41d,17d,visible)
        vctr(0d,8d,hidden)
        vctr(-42d,-25d,visible)
        vctr(41d,-16d,visible)
        vctr(0d,-10d,hidden)
        vctr(-41d,28d,visible)
        vctr(41d,0d,visible)
        rtsl
        ;.db $11,$00,$29,$20,$00,$44,$E7,$1F,$D6,$3F,$F0,$1F,$29,$20,$00,$5B
        ;.db $1C,$00,$D7,$3F,$00,$00,$29,$20,$00,$C0
        
lhorz38 vctr(45d,0d,visible)
        rtsl
        ;.db $00,$00,$2D,$20,$00,$C0
        
lhorz40 vctr(-45d,0d,visible)
        rtsl
        ;.db $00,$00,$D3,$3F,$00,$C0
        
lhorz41 vctr(-41d,17d,visible)
        vctr(2d,8d,hidden)
        vctr(39d,-25d,visible)
        vctr(-44d,-16d,visible)
        vctr(0d,-10d,hidden)
        vctr(43d,28d,visible)
        vctr(-40d,0d,visible)
        rtsl
        ;.db $11,$00,$D7,$3F,$01,$44,$E7,$1F,$27,$20,$F0,$1F,$D4,$3F,$00,$5B
        ;.db $1C,$00,$2B,$20,$00,$00,$D8,$3F,$00,$C0
        
lhorz42 vctr(-31d,32d,visible)
        vctr(2d,14d,hidden)
        vctr(28d,-46d,visible)
        vctr(-35d,-30d,visible)
        vctr(0d,-22d,hidden)
        vctr(34d,53d,visible)
        vctr(-28d,0d,visible)
        rtsl
        ;.db $20,$00,$E1,$3F,$01,$47,$D2,$1F,$1C,$20,$E2,$1F,$DD,$3F,$00,$55
        ;.db $35,$00,$22,$20,$32,$40,$00,$C0
        
lhorz43 vctr(-17d,41d,visible)
        vctr(2d,16d,hidden)
        vctr(14d,-59d,visible)
        vctr(-20d,-40d,visible)
        vctr(0d,-28d,hidden)
        vctr(19d,70d,visible)
        vctr(-14d,0d,visible)
        rtsl
        ;.db $29,$00,$EF,$3F,$01,$48,$C5,$1F,$0E,$20,$D8,$1F,$EC,$3F,$00,$52
        ;.db $46,$00,$13,$20,$39,$40,$00,$C0

;Rotation of Spinner horizontally        
lhorz44 vctr(0d,45d,visible)
        vctr(0d,18d,hidden)
        vctr(0d,-63d,visible)
        vctr(0d,-44d,visible)
        vctr(0d,-32d,hidden)
        vctr(0d,76d,visible)
        vctr(2d,0d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$00,$49,$C1,$1F,$00,$20,$D4,$1F,$00,$20,$E0,$1F
        ;.db $00,$00,$4C,$00,$00,$20,$21,$40,$00,$C0
        
lhorz45 vctr(0d,45d,visible)
        vctr(-6d,8d,hidden)
        vctr(7d,-55d,visible)
        vctr(7d,-34d,visible)
        vctr(-2d,-35d,hidden)
        vctr(-4d,70d,visible)
        vctr(-4d,-6d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$1D,$44,$C9,$1F,$07,$20,$DE,$1F,$07,$20,$DD,$1F
        ;.db $FE,$1F,$46,$00,$FC,$3F,$3E,$5D,$00,$C0
        
lhorz46 vctr(0d,45d,visible)
        vctr(-14d,0d,hidden)
        vctr(14d,-46d,visible)
        vctr(14d,-24d,visible)
        vctr(-5d,-38d,hidden)
        vctr(-8d,63d,visible)
        vctr(-8d,-12d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$19,$40,$D2,$1F,$0E,$20,$27,$54,$DA,$1F,$FB,$1F
        ;.db $3F,$00,$F8,$3F,$3C,$5A,$00,$C0
        
lhorz47 vctr(0d,45d,visible)
        vctr(-20d,-6d,hidden)
        vctr(21d,-38d,visible)
        vctr(20d,-14d,visible)
        vctr(-8d,-41d,hidden)
        vctr(-13d,56d,visible)
        vctr(-12d,-18d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$16,$5D,$DA,$1F,$15,$20,$2A,$59,$D7,$1F,$F8,$1F
        ;.db $38,$00,$F3,$3F,$3A,$57,$00,$C0
        
lhorz48 vctr(0d,45d,visible)
        vctr(-28d,-14d,hidden)
        vctr(28d,-30d,visible)
        vctr(28d,-4d,visible)
        vctr(-10d,-44d,hidden)
        vctr(-17d,50d,visible)
        vctr(-16d,-24d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$12,$59,$2E,$51,$2E,$5E,$D4,$1F,$F6,$1F,$32,$00
        ;.db $EF,$3F,$38,$54,$00,$C0
        
lhorz50 vctr(0d,45d,visible)
        vctr(-35d,-23d,hidden)
        vctr(35d,-22d,visible)
        vctr(35d,4d,visible)
        vctr(-13d,-47d,hidden)
        vctr(-22d,43d,visible)
        vctr(-22d,-30d,visible)
        rtsl
        ;.db $2D,$00,$00,$20,$E9,$1F,$DD,$1F,$EA,$1F,$23,$20,$04,$00,$23,$20
        ;.db $D1,$1F,$F3,$1F,$2B,$00,$EA,$3F,$35,$51,$00,$C0
    
;************************************** 
;* Side View Spinner Rotation Frames  *
;************************************** 
spinnr7 = $60|page 

spinner0    vctr(-60d,19d,visible)
            vctr(-24d,48d,visible)
            vctr(27d,44d,visible)
            vctr(-67d,-132d,hidden)
            vctr(33d,-40d,visible)
            vctr(53d,8d,visible)
            vctr(37d,51d,visible)
            vctr(37d,-51d,visible)
            vctr(-8d,-53d,visible)
            vctr(-48d,-19d,visible)
            vctr(132d,67d,hidden)
            vctr(-3d,52d,visible)
            vctr(-48d,24d,visible)
            vctr(-60d,-19d,visible)
            vctr(0d,64d,visible)
            vctr(38d,38d,visible)
            vctr(51d,-12d,visible)
            rtsl
        
spinner1    vctr(-19d,-60d,visible)
            vctr(-48d,-24d,visible)
            vctr(-44d,27d,visible)
            vctr(132d,-67d,hidden)
            vctr(40d,33d,visible)
            vctr(-8d,53d,visible)
            vctr(-51d,37d,visible)
            vctr(51d,37d,visible)
            vctr(53d,-8d,visible)
            vctr(19d,-48d,visible)
            vctr(-67d,132d,hidden)
            vctr(-52d,-3d,visible)
            vctr(-24d,-48d,visible)
            vctr(19d,-60d,visible)
            vctr(-64d,0d,visible)
            vctr(-38d,38d,visible)
            vctr(12d,51d,visible)
            rtsl
        
spinner2    vctr(60d,-19d,visible)
            vctr(24d,-48d,visible)
            vctr(-27d,-44d,visible)
            vctr(67d,132d,hidden)
            vctr(-33d,40d,visible)
            vctr(-53d,-8d,visible)
            vctr(-37d,-51d,visible)
            vctr(-37d,51d,visible)
            vctr(8d,53d,visible)
            vctr(48d,19d,visible)
            vctr(-132d,-67d,hidden)
            vctr(3d,-52d,visible)
            vctr(48d,-24d,visible)
            vctr(60d,19d,visible)
            vctr(0d,-64d,visible)
            vctr(-38d,-38d,visible)
            vctr(-51d,12d,visible)
            rtsl
        
spinner3    vctr(19d,60d,visible)
            vctr(48d,24d,visible)
            vctr(44d,-27d,visible)
            vctr(-132d,67d,hidden)
            vctr(-40d,-33d,visible)
            vctr(8d,-53d,visible)
            vctr(51d,-37d,visible)
            vctr(-51d,-37d,visible)
            vctr(-53d,8d,visible)
            vctr(-19d,48d,visible)
            vctr(67d,-132d,hidden)
            vctr(52d,3d,visible)
            vctr(24d,48d,visible)
            vctr(-19d,60d,visible)
            vctr(64d,0d,visible)
            vctr(38d,-38d,visible)
            vctr(-12d,-51d,visible)
            rtsl
        
;***************************************    
;* Web Spinner Explosions              *
;***************************************    
cerexp0     vctr(5d,1d,visible)
            vctr(1d,0d,hidden)
            vctr(9d,1d,visible)
            vctr(2d,0d,hidden)
            vctr(21d,3d,visible)
            vctr(2d,0d,hidden)
            vctr(4d,1d,visible)
            vctr(-44d,-5d,hidden)
            rtsl
            ;.db $01,$00,$05,$20,$00,$00,$01,$00,$01,$00,$09,$20,$01,$40,$03,$00
            ;.db $15,$20,$01,$40,$01,$00,$04,$20,$FB,$1F,$D4,$1F,$00,$C0
        
cerexp1     vctr(3d,1d,hidden)
            vctr(10d,3d,visible)
            vctr(1d,0d,hidden)
            vctr(22d,6d,visible)
            vctr(5d,2d,visible)
            vctr(-31d,-7d,hidden)
            vctr(6d,3d,visible)
            vctr(8d,6d,visible)
            vctr(6d,6d,visible)
            vctr(17d,-3d,hidden)
            vctr(3d,2d,visible)
            vctr(10d,-1d,hidden)
            vctr(2d,1d,visible)
            vctr(-62d,-19d,hidden)
            rtsl
            ;.db $01,$00,$03,$00,$03,$00,$0A,$20,$00,$00,$01,$00,$2B,$43,$02,$00
            ;.db $05,$20,$F9,$1F,$E1,$1F,$03,$00,$06,$20,$24,$43,$23,$43,$FD,$1F
            ;.db $11,$00,$02,$00,$03,$20,$FF,$1F,$0A,$00,$01,$00,$02,$20,$ED,$1F
            ;.db $C2,$1F,$00,$C0
        
cerexp2     vctr(0d,1d,hidden)
            vctr(15d,7d,visible)
            vctr(1d,1d,hidden)
            vctr(9d,6d,visible)
            vctr(1d,1d,hidden)
            vctr(7d,10d,visible)
            vctr(1d,0d,hidden)
            vctr(2d,4d,visible)
            vctr(-20d,-24d,hidden)
            vctr(5d,2d,visible)
            vctr(3d,1d,hidden)
            vctr(6d,3d,visible)
            vctr(5d,4d,visible)
            vctr(19d,14d,hidden)
            vctr(3d,2d,visible)
            vctr(5d,-5d,hidden)
            vctr(2d,1d,visible)
            vctr(-64d,-28d,hidden)
            rtsl
            ;.db $01,$00,$00,$00,$07,$00,$0F,$20,$01,$00,$01,$00,$06,$00,$09,$20
            ;.db $01,$00,$01,$00,$0A,$00,$07,$20,$00,$00,$01,$00,$21,$42,$16,$54
            ;.db $02,$00,$05,$20,$01,$00,$03,$00,$03,$00,$06,$20,$04,$00,$05,$20
            ;.db $0E,$00,$13,$00,$02,$00,$03,$20,$FB,$1F,$05,$00,$01,$00,$02,$20
            ;.db $E4,$1F,$C0,$1F,$00,$C0
        
cerexp3     vctr(14d,11d,visible)
            vctr(10d,11d,visible)
            vctr(1d,7d,visible)
            vctr(-3d,5d,visible)
            vctr(-5d,4d,visible)
            vctr(-14d,4d,hidden)
            vctr(-3d,0d,visible)
            vctr(27d,-3d,hidden)
            vctr(4d,-4d,visible)
            vctr(3d,-6d,visible)
            vctr(3d,1d,hidden)
            vctr(-6d,-9d,visible)
            vctr(-6d,-6d,visible)
            vctr(-8d,-5d,visible)
            vctr(-17d,-10d,hidden)
            rtsl
            ;.db $0B,$00,$0E,$20,$0B,$00,$0A,$20,$07,$00,$01,$20,$05,$00,$FD,$3F
            ;.db $04,$00,$FB,$3F,$19,$42,$00,$00,$FD,$3F,$FD,$1F,$1B,$00,$22,$5E
            ;.db $FA,$1F,$03,$20,$01,$00,$03,$00,$F7,$1F,$FA,$3F,$3D,$5D,$FB,$1F
            ;.db $F8,$3F,$F6,$1F,$EF,$1F,$00,$C0
        
cerexp4     vctr(29d,18d,hidden)
            vctr(0d,5d,visible)
            vctr(1d,6d,visible)
            vctr(3d,6d,visible)
            vctr(4d,4d,visible)
            vctr(2d,2d,hidden)
            vctr(9d,5d,visible)
            vctr(11d,3d,visible)
            vctr(-24d,-5d,hidden)
            vctr(-3d,-3d,visible)
            vctr(-4d,-5d,visible)
            vctr(-2d,-5d,visible)
            vctr(-26d,-30d,hidden)
            rtsl
            ;.db $12,$00,$1D,$00,$05,$00,$00,$20,$06,$00,$01,$20,$06,$00,$03,$20
            ;.db $22,$42,$01,$41,$05,$00,$09,$20,$03,$00,$0B,$20,$FB,$1F,$E8,$1F
            ;.db $FD,$1F,$FD,$3F,$FB,$1F,$FC,$3F,$FB,$1F,$FE,$3F,$13,$51,$00,$C0
        
cerexp5     vctr(18d,37d,hidden)
            vctr(1d,3d,visible)
            vctr(2d,3d,visible)
            vctr(4d,-3d,hidden)
            vctr(3d,4d,visible)
            vctr(-5d,2d,hidden)
            vctr(6d,5d,visible)
            vctr(7d,4d,visible)
            vctr(3d,-9d,hidden)
            vctr(8d,4d,visible)
            vctr(9d,2d,visible)
            vctr(-56d,-52d,hidden)
            rtsl
            ;.db $25,$00,$12,$00,$03,$00,$01,$20,$03,$00,$02,$20,$FD,$1F,$04,$00
            ;.db $04,$00,$03,$20,$02,$00,$FB,$1F,$05,$00,$06,$20,$04,$00,$07,$20
            ;.db $F7,$1F,$03,$00,$24,$42,$02,$00,$09,$20,$CC,$1F,$C8,$1F,$00,$C0
        
cerexp6     vctr(12d,38d,hidden)
            vctr(2d,4d,visible)
            vctr(6d,7d,hidden)
            vctr(4d,4d,visible)
            vctr(4d,3d,visible)
            vctr(10d,5d,hidden)
            vctr(4d,2d,visible)
            vctr(-42d,-63d,hidden)
            rtsl
            ;.db $26,$00,$0C,$00,$21,$42,$07,$00,$06,$00,$22,$42,$03,$00,$04,$20
            ;.db $05,$00,$0A,$00,$22,$41,$C1,$1F,$D6,$1F,$00,$C0
        
cerexp7     vctr(8d,40d,hidden)
            vctr(1d,2d,visible)
            vctr(9d,14d,hidden)
            vctr(4d,3d,visible)
            vctr(21d,8d,hidden)
            vctr(3d,2d,visible)
            vctr(-46d,-69d,hidden)
            rtsl
            ;.db $28,$00,$08,$00,$02,$00,$01,$20,$0E,$00,$09,$00,$03,$00,$04,$20
            ;.db $08,$00,$15,$00,$02,$00,$03,$20,$BB,$1F,$D2,$1F,$00,$C0

;**************************************     
            vctr(-42d,0d,hidden)
            jsrl(pupeye)
            jsrl(pupeye_)
            vctr(-6d,0d,hidden)
            rtsl
            ;.db $00,$00,$D6,$1F,$8C,$BF,$8A,$BF,$1D,$40,$00,$C0
        
            vctr(-34d,0d,hidden)
            jsrl(pupeye)
            jsrl(pupeye_)
            vctr(-14d,0d,hidden)
            rtsl
            ;.db $00,$00,$DE,$1F,$8C,$BF,$8A,$BF,$19,$40,$00,$C0
        
            vctr(-26d,0d,hidden)
            jsrl(pupeye)
            jsrl(pupeye_)
            vctr(-22d,0d,hidden)
            rtsl         
            ;.db $13,$40,$8C,$BF,$8A,$BF,$15,$40,$00,$C0
        
pupeye_     vctr(48d,0d,hidden)
            ;.db $00,$00,$30,$00
            
pupeye      vctr(10d,20d,visible)
            vctr(10d,-20d,visible)
            vctr(-10d,-20d,visible)
            vctr(-10d,20d,visible)
            rtsl
            ;.db $25,$4A,$25,$56,$3B,$56,$3B,$4A,$00,$C0
        
pupeyeb     vctr(24d,50d,visible)
            vctr(24d,-50d,visible)
            vctr(-24d,-50d,visible)
            vctr(-24d,50d,visible)
            rtsl
            ;.db $32,$00,$18,$20,$CE,$1F,$18,$20,$CE,$1F,$E8,$3F,$32,$00,$E8,$3F
            ;.db $00,$C0
        
pupw1       jsrl(pupeyeb)
            vctr(-48d,0d,hidden)
            jsrl(pupeyeb)
            vctr(12d,26d,hidden)
            vctr(-64d,30d,visible)
            rtsl
            ;.db $91,$BF,$00,$00,$D0,$1F,$91,$BF,$06,$4D,$1E,$00,$C0,$3F,$00,$C0
        
pupw2       vctr(24d,-20d,visible)
            vctr(52d,0d,visible)
            vctr(12d,16d,visible)
            vctr(0d,28d,visible)
            vctr(-12d,22d,visible)
            rtsl
            ;.db $2C,$56,$00,$00,$34,$20,$26,$48,$20,$4E,$3A,$4B,$00,$C0
        
pup0        jsrl(pupw1)
            vctr(26d,-52d,visible)
            vctr(20d,-14d,hidden)
            vctr(-58d,40d,visible)
            vctr(78d,-80d,visible)
            vctr(-28d,28d,hidden)
            vctr(22d,-48d,visible)
            jsrl(pupw2)
            vctr(8d,-12d,hidden)
            vctr(52d,66d,visible)
            vctr(-50d,-24d,visible)
            rtsl
            ;.db $9A,$BF,$CC,$1F,$1A,$20,$0A,$59,$28,$00,$C6,$3F,$B0,$1F,$4E,$20
            ;.db $12,$4E,$D0,$1F,$16,$20,$A2,$BF,$04,$5A,$42,$00,$34,$20,$E8,$1F
            ;.db $CE,$3F,$00,$C0
        
pup1        jsrl(pupw1)
            vctr(32d,-66d,visible)
            vctr(14d,0d,hidden)
            vctr(-66d,0d,visible)
            vctr(86d,-40d,visible)
            vctr(-18d,8d,hidden)
            vctr(12d,-28d,visible)
            jsrl(pupw2)
            vctr(10d,-18d,hidden)
            vctr(58d,32d,visible)
            vctr(-62d,0d,visible)
            rtsl
            ;.db $9A,$BF,$BE,$1F,$20,$20,$07,$40,$00,$00,$BE,$3F,$D8,$1F,$56,$20
            ;.db $17,$44,$26,$52,$A2,$BF,$05,$57,$20,$00,$3A,$20,$00,$00,$C2,$3F
            ;.db $00,$C0
        
pup2        jsrl(pupw1)
            vctr(36d,-74d,visible)
            vctr(10d,8d,hidden)
            vctr(-62d,-40d,visible)
            vctr(82d,0d,visible)
            vctr(-16d,0d,visible)
            vctr(10d,-20d,visible)
            jsrl(pupw2)
            vctr(12d,-26d,hidden)
            vctr(52d,0d,visible)
            vctr(-62d,30d,visible)
            rtsl
            ;.db $9A,$BF,$B6,$1F,$24,$20,$05,$44,$D8,$1F,$C2,$3F,$00,$00,$52,$20
            ;.db $38,$40,$25,$56,$A2,$BF,$06,$53,$00,$00,$34,$20,$1E,$00,$C2,$3F
            ;.db $00,$C0
            
pup3        jsrl(pupw1)
            vctr(38d,-78d,visible)
            vctr(8d,12d,hidden)
            vctr(-52d,-80d,visible)
            vctr(72d,40d,visible)
            vctr(-12d,-6d,hidden)
            vctr(6d,-14d,visible)
            jsrl(pupw2)
            vctr(12d,-36d,hidden)
            vctr(44d,-30d,visible)
            vctr(-46d,48d,visible)
            rtsl
  
            ;.db $9A,$BF,$B2,$1F,$26,$20,$04,$46,$B0,$1F,$CC,$3F,$28,$00,$48,$20
            ;.db $1A,$5D,$23,$59,$A2,$BF,$DC,$1F,$0C,$00,$E2,$1F,$2C,$20,$30,$00
            ;.db $D2,$3F,$00,$C0
        		
		
	.org $7fff
			
cksum3	.byte $03

	.end

;**************************************
;* VROM Page 2 exports
.export mapdot,shipsh,maze1,maze2,maze3,maze4,maze5,maze6,maze7
.export mape1,mape2,mape3,mape4,mape5,mape6,mape7
.export rod0,rod1,rod2,rod3
.export head0,head1,head2,head3,head4,head5,head6
.export tail0,tail1,gun0,gun1,gun2,gun3,eye0,eye1,eye2
.export body,bodyt
.export mount,lgun0,lgun1,lgun2,lgun3
.export brl00,brl01,brl02,brl10,brl11,brl12,brl20,brl21,brl22,brl30,brl31,brl32
.export laz00,laz01,laz02,laz03,laz10,laz20,laz30,laz31,laz32,laz33
.export robothead0,robothead1,robothead2,robothead3,robothead4,robothead5,robothead6,robothead7
.export robotbody0,robotbody1,robotbody2,robotbody3,robotbody4,robotbody5,robotbody6,robotbody7
.export levt0,levt1,levt2,levt3
.export beacn0,beacn1,beacn2,beacn3,beacn4,beacn5,beacn6,beacn7

.export spinner0,spinner1,spinner2,spinner3  

.export lvert00,lvert01,lvert02,lvert03,lvert04,lvert05,lvert06,lvert07,lvert08
.export lvert10,lvert11,lvert12,lvert13,lvert14,lvert15,lvert16,lvert17,lvert18
.export lvert20,lvert21,lvert22,lvert23,lvert24,lvert25,lvert26,lvert27,lvert28
.export lvert30,lvert31,lvert32,lvert33,lvert34,lvert35,lvert36,lvert37,lvert38
.export lvert40,lvert41,lvert42,lvert43,lvert44,lvert45,lvert46,lvert47,lvert48
.export lvert50,lvert51,lvert52,lvert53,lvert54,lvert55,lvert56,lvert57,lvert58

.export hfold20,hfold21,hfold22,hfold23,hfold24

.export lhorz00,lhorz01,lhorz02,lhorz03,lhorz04,lhorz05,lhorz06,lhorz07,lhorz08
.export lhorz10,lhorz11,lhorz12,lhorz13,lhorz14,lhorz15,lhorz16,lhorz17,lhorz18
.export lhorz20,lhorz21,lhorz22,lhorz23,lhorz24,lhorz25,lhorz26,lhorz27,lhorz28
.export lhorz30,lhorz31,lhorz32,lhorz33,lhorz34,lhorz35,lhorz36,lhorz37,lhorz38
.export lhorz40,lhorz41,lhorz42,lhorz43,lhorz44,lhorz45,lhorz46,lhorz47,lhorz48
.export lhorz50

.export cerexp0,cerexp1,cerexp2,cerexp3,cerexp4,cerexp5,cerexp6,cerexp7

.export webln00,webln01,webln02,webln03,webln04,webln05,webln06
.export webln07,webln08,webln09,webln0a,webln0b

.export weblnh0,weblnh1,weblnh2,weblnh3,weblnh4,weblnh5,weblnh6
.export weblnh7,weblnh8,weblnh9,weblnha,weblnhb

.export live7,maz7,mapdt7,rods7,body7,gun7,mzls7,becn7,shpsh7