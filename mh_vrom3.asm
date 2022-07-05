;********************************************
;* Major Havoc Vector ROM Overlay Page 3    *
;********************************************
	.title "TWOV3"
;********************************************
;* Includes:                                *
;********************************************
#include "vector.ah"	;For the various vctr macros

;********************************************
;* Program:                                 *
;********************************************
	.org $6000
		
page = 3

		.dw $E8C8 	;4th Vector ROM Page
		
;********************************************

tacct7 = $60|page
		
tactc0  vctr(-48d,49d,hidden)
        vctr(97d,0d,visible)
        vctr(0d,-96d,visible)
        vctr(-97d,-1d,visible)
        vctr(0d,97d,visible)
        vctr(22d,-23d,visible)
        vctr(52d,-1d,visible)
        vctr(22d,24d,hidden)
        vctr(-22d,-23d,visible)
        vctr(0d,-52d,visible)
        vctr(23d,-21d,visible)
        vctr(-23d,21d,hidden)
        vctr(-52d,1d,visible)
        vctr(-22d,-22d,visible)
        vctr(22d,21d,hidden)
        vctr(0d,52d,visible)
        vctr(26d,152d,hidden)
        vctr(15d,-9d,visible)
        vctr(0d,-17d,visible)
        vctr(-15d,-7d,visible)
        vctr(-13d,7d,visible)
        vctr(0d,18d,visible)
        vctr(13d,8d,visible)
        vctr(145d,-81d,hidden)
        vctr(14d,-9d,visible)
        vctr(0d,-15d,visible)
        vctr(-13d,-9d,visible)
        vctr(-15d,9d,visible)
        vctr(0d,15d,visible)
        vctr(14d,9d,visible)
        vctr(0d,-162d,hidden)
        vctr(14d,-8d,visible)
        vctr(-1d,-16d,visible)
        vctr(-12d,-8d,visible)
        vctr(-15d,8d,visible)
        vctr(0d,16d,visible)
        vctr(14d,8d,visible)
        vctr(-145d,-79d,hidden)
        vctr(15d,-10d,visible)
        vctr(-1d,-17d,visible)
        vctr(-14d,-6d,visible)
        vctr(-14d,7d,visible)
        vctr(1d,17d,visible)
        vctr(13d,8d,visible)
        vctr(-145d,81d,hidden)
        vctr(14d,-9d,visible)
        vctr(0d,-15d,visible)
        vctr(-14d,-9d,visible)
        vctr(-15d,8d,visible)
        vctr(0d,16d,visible)
        vctr(15d,9d,visible)
        vctr(1d,161d,hidden)
        vctr(13d,-9d,visible)
        vctr(0d,-15d,visible)
        vctr(-13d,-8d,visible)
        vctr(-15d,8d,visible)
        vctr(0d,15d,visible)
        vctr(15d,9d,visible)
        vctr(144d,-97d,hidden)
        rtsl
        ;.db $31,$00,$D0,$1F,$00,$00,$61,$20,$A0,$1F,$00,$20,$FF,$1F,$9F,$3F
        ;.db $61,$00,$00,$20,$E9,$1F,$16,$20,$FF,$1F,$34,$20,$0B,$4C,$E9,$1F
        ;.db $EA,$3F,$CC,$1F,$00,$20,$EB,$1F,$17,$20,$15,$00,$E9,$1F,$01,$00
        ;.db $CC,$3F,$35,$55,$15,$00,$16,$00,$34,$00,$00,$20,$98,$00,$1A,$00
        ;.db $F7,$1F,$0F,$20,$EF,$1F,$00,$20,$F9,$1F,$F1,$3F,$07,$00,$F3,$3F
        ;.db $20,$49,$08,$00,$0D,$20,$AF,$1F,$91,$00,$F7,$1F,$0E,$20,$F1,$1F
        ;.db $00,$20,$F7,$1F,$F3,$3F,$09,$00,$F1,$3F,$0F,$00,$00,$20,$09,$00
        ;.db $0E,$20,$5E,$1F,$00,$00,$27,$5C,$F0,$1F,$FF,$3F,$3A,$5C,$08,$00
        ;.db $F1,$3F,$20,$48,$27,$44,$B1,$1F,$6F,$1F,$F6,$1F,$0F,$20,$EF,$1F
        ;.db $FF,$3F,$39,$5D,$07,$00,$F2,$3F,$11,$00,$01,$20,$08,$00,$0D,$20
        ;.db $51,$00,$6F,$1F,$F7,$1F,$0E,$20,$F1,$1F,$00,$20,$F7,$1F,$F2,$3F
        ;.db $08,$00,$F1,$3F,$20,$48,$09,$00,$0F,$20,$A1,$00,$01,$00,$F7,$1F
        ;.db $0D,$20,$F1,$1F,$00,$20,$F8,$1F,$F3,$3F,$08,$00,$F1,$3F,$0F,$00
        ;.db $00,$20,$09,$00,$0F,$20,$9F,$1F,$90,$00,$00,$C0
        
tactc1  vctr(-161d,32d,visible)
        vctr(0d,162d,visible)
        vctr(161d,32d,visible)
        vctr(161d,-32d,visible)
        vctr(0d,-162d,visible)
        vctr(-161d,-33d,visible)
        vctr(-161d,33d,hidden)
        vctr(96d,32d,visible)
        vctr(130d,0d,visible)
        vctr(-65d,-15d,visible)
        vctr(-65d,16d,visible)
        vctr(0d,96d,visible)
        vctr(65d,17d,visible)
        vctr(64d,-17d,visible)
        vctr(0d,-96d,visible)
        vctr(97d,-33d,visible)
        vctr(-322d,162d,hidden)
        vctr(96d,-33d,visible)
        vctr(129d,0d,visible)
        vctr(97d,32d,visible)
        vctr(-136d,-138d,hidden)
        vctr(8d,-6d,visible)
        vctr(-66d,1d,visible)
        vctr(8d,4d,visible)
        vctr(-132d,-123d,hidden)
        vctr(55d,0d,visible)
        vctr(-27d,-29d,visible)
        vctr(-28d,29d,visible)
        vctr(33d,-11d,hidden)
        vctr(3d,-3d,visible)
        vctr(-7d,-9d,visible)
        vctr(-7d,9d,visible)
        vctr(1d,4d,visible)
        vctr(106d,10d,hidden)
        vctr(56d,0d,visible)
        vctr(-28d,-28d,visible)
        vctr(-28d,28d,visible)
        vctr(34d,-11d,hidden)
        vctr(3d,-4d,visible)
        vctr(-9d,-8d,visible)
        vctr(-7d,8d,visible)
        vctr(2d,4d,visible)
        vctr(106d,11d,hidden)
        vctr(56d,0d,visible)
        vctr(-28d,-27d,visible)
        vctr(-28d,27d,visible)
        vctr(34d,-11d,hidden)
        vctr(3d,-4d,visible)
        vctr(-8d,-8d,visible)
        vctr(-7d,8d,visible)
        vctr(3d,4d,visible)
        vctr(-218d,-53d,hidden)
        vctr(56d,0d,visible)
        vctr(-28d,-29d,visible)
        vctr(-28d,29d,visible)
        vctr(32d,-12d,hidden)
        vctr(4d,-4d,visible)
        vctr(-8d,-8d,visible)
        vctr(-7d,8d,visible)
        vctr(2d,4d,visible)
        vctr(105d,12d,hidden)
        vctr(57d,0d,visible)
        vctr(-28d,-29d,visible)
        vctr(-29d,28d,visible)
        vctr(33d,-11d,hidden)
        vctr(3d,-4d,visible)
        vctr(-7d,-8d,visible)
        vctr(-7d,8d,visible)
        vctr(2d,4d,visible)
        vctr(-88d,-53d,hidden)
        vctr(56d,0d,visible)
        vctr(-28d,-28d,visible)
        vctr(-28d,29d,visible)
        vctr(32d,-12d,hidden)
        vctr(5d,-5d,visible)
        vctr(-9d,-8d,visible)
        vctr(-6d,8d,visible)
        vctr(1d,5d,visible)
        vctr(5d,209d,hidden)
        rtsl
        ;.db $20,$00,$5F,$3F,$A2,$00,$00,$20,$20,$00,$A1,$20,$E0,$1F,$A1,$20
        ;.db $5E,$1F,$00,$20,$DF,$1F,$5F,$3F,$21,$00,$5F,$1F,$20,$00,$60,$20
        ;.db $00,$00,$82,$20,$F1,$1F,$BF,$3F,$10,$00,$BF,$3F,$60,$00,$00,$20
        ;.db $11,$00,$41,$20,$EF,$1F,$40,$20,$A0,$1F,$00,$20,$DF,$1F,$61,$20
        ;.db $A2,$00,$BE,$1E,$DF,$1F,$60,$20,$00,$00,$81,$20,$20,$00,$61,$20
        ;.db $76,$1F,$78,$1F,$24,$5D,$01,$00,$BE,$3F,$24,$42,$85,$1F,$7C,$1F
        ;.db $00,$00,$37,$20,$E3,$1F,$E5,$3F,$1D,$00,$E4,$3F,$F5,$1F,$21,$00
        ;.db $FD,$1F,$03,$20,$F7,$1F,$F9,$3F,$09,$00,$F9,$3F,$04,$00,$01,$20
        ;.db $0A,$00,$6A,$00,$00,$00,$38,$20,$32,$52,$32,$4E,$F5,$1F,$22,$00
        ;.db $FC,$1F,$03,$20,$F8,$1F,$F7,$3F,$08,$00,$F9,$3F,$21,$42,$0B,$00
        ;.db $6A,$00,$00,$00,$38,$20,$E5,$1F,$E4,$3F,$1B,$00,$E4,$3F,$F5,$1F
        ;.db $22,$00,$FC,$1F,$03,$20,$3C,$5C,$08,$00,$F9,$3F,$04,$00,$03,$20
        ;.db $CB,$1F,$26,$1F,$00,$00,$38,$20,$E3,$1F,$E4,$3F,$1D,$00,$E4,$3F
        ;.db $F4,$1F,$20,$00,$22,$5E,$3C,$5C,$08,$00,$F9,$3F,$21,$42,$0C,$00
        ;.db $69,$00,$00,$00,$39,$20,$E3,$1F,$E4,$3F,$1C,$00,$E3,$3F,$F5,$1F
        ;.db $21,$00,$FC,$1F,$03,$20,$F8,$1F,$F9,$3F,$08,$00,$F9,$3F,$21,$42
        ;.db $CB,$1F,$A8,$1F,$00,$00,$38,$20,$32,$52,$1D,$00,$E4,$3F,$F4,$1F
        ;.db $20,$00,$FB,$1F,$05,$20,$F8,$1F,$F7,$3F,$3D,$44,$05,$00,$01,$20
        ;.db $D1,$00,$05,$00,$00,$C0
        
tactc2  vctr(-84d,0d,hidden)
        vctr(45d,68d,visible)
        vctr(80d,1d,visible)
        vctr(45d,-69d,visible)
        vctr(-45d,-74d,visible)
        vctr(-80d,1d,visible)
        vctr(-44d,73d,visible)
        vctr(54d,0d,visible)
        vctr(15d,-24d,visible)
        vctr(-25d,-48d,visible)
        vctr(25d,48d,hidden)
        vctr(28d,0d,visible)
        vctr(27d,-49d,visible)
        vctr(-27d,49d,hidden)
        vctr(15d,24d,visible)
        vctr(57d,0d,visible)
        vctr(-57d,0d,hidden)
        vctr(-15d,23d,visible)
        vctr(26d,45d,visible)
        vctr(-25d,-45d,hidden)
        vctr(-28d,0d,visible)
        vctr(-26d,45d,visible)
        vctr(26d,-45d,hidden)
        vctr(-16d,-23d,visible)
        vctr(-27d,-138d,hidden)
        vctr(-88d,51d,visible)
        vctr(-1d,159d,visible)
        vctr(145d,82d,visible)
        vctr(145d,-82d,visible)
        vctr(1d,-161d,visible)
        vctr(-88d,-49d,visible)
        vctr(-67d,65d,hidden)
        vctr(9d,-8d,visible)
        vctr(13d,8d,visible)
        vctr(45d,117d,hidden)
        vctr(11d,-5d,visible)
        vctr(-1d,-13d,visible)
        vctr(-136d,0d,hidden)
        vctr(0d,15d,visible)
        vctr(12d,4d,visible)
        vctr(56d,-45d,hidden)
        rtsl
        ;.db $00,$00,$AC,$1F,$44,$00,$2D,$20,$01,$00,$50,$20,$BB,$1F,$2D,$20
        ;.db $B6,$1F,$D3,$3F,$01,$00,$B0,$3F,$49,$00,$D4,$3F,$00,$00,$36,$20
        ;.db $E8,$1F,$0F,$20,$D0,$1F,$E7,$3F,$30,$00,$19,$00,$2E,$40,$CF,$1F
        ;.db $1B,$20,$31,$00,$E5,$1F,$18,$00,$0F,$20,$00,$00,$39,$20,$00,$00
        ;.db $C7,$1F,$17,$00,$F1,$3F,$2D,$00,$1A,$20,$D3,$1F,$E7,$1F,$32,$40
        ;.db $2D,$00,$E6,$3F,$D3,$1F,$1A,$00,$E9,$1F,$F0,$3F,$76,$1F,$E5,$1F
        ;.db $33,$00,$A8,$3F,$9F,$00,$FF,$3F,$52,$00,$91,$20,$AE,$1F,$91,$20
        ;.db $5F,$1F,$01,$20,$CF,$1F,$A8,$3F,$41,$00,$BD,$1F,$F8,$1F,$09,$20
        ;.db $08,$00,$0D,$20,$75,$00,$2D,$00,$FB,$1F,$0B,$20,$F3,$1F,$FF,$3F
        ;.db $00,$00,$78,$1F,$0F,$00,$00,$20,$26,$42,$D3,$1F,$38,$00,$00,$C0

tactc3  vctr(-48d,96d,hidden)
        vctr(0d,-192d,visible)
        vctr(96d,0d,visible)
        vctr(0d,192d,visible)
        vctr(-192d,0d,visible)
        vctr(0d,288d,visible)
        vctr(-128d,0d,visible)
        vctr(0d,-768d,visible)
        vctr(128d,0d,visible)
        vctr(0d,96d,visible)
        vctr(-128d,256d,hidden)
        vctr(128d,0d,visible)
        vctr(0d,-160d,visible)
        vctr(96d,0d,visible)
        vctr(0d,-96d,visible)
        vctr(96d,0d,hidden)
        vctr(0d,96d,visible)
        vctr(224d,0d,visible)
        vctr(-96d,-96d,hidden)
        vctr(0d,-96d,visible)
        vctr(-224d,0d,hidden)
        vctr(320d,0d,visible)
        vctr(0d,768d,visible)
        vctr(-320d,0d,visible)
        vctr(0d,-192d,visible)
        vctr(208d,0d,visible)
        vctr(-96d,96d,hidden)
        vctr(208d,0d,visible)
        vctr(-160d,-192d,hidden)
        vctr(96d,0d,visible)
        vctr(0d,-192d,visible)
        vctr(-96d,0d,visible)
        vctr(0d,192d,visible)
        vctr(-112d,-96d,hidden)
        rtsl
        ;.db $60,$00,$D0,$1F,$40,$1F,$00,$20,$00,$00,$60,$20,$C0,$00,$00,$20
        ;.db $00,$00,$40,$3F,$20,$01,$00,$20,$00,$00,$80,$3F,$00,$1D,$00,$20
        ;.db $00,$00,$80,$20,$60,$00,$00,$20,$00,$01,$80,$1F,$00,$00,$80,$20
        ;.db $60,$1F,$00,$20,$00,$00,$60,$20,$A0,$1F,$00,$20,$00,$00,$60,$00
        ;.db $60,$00,$00,$20,$00,$00,$E0,$20,$A0,$1F,$A0,$1F,$A0,$1F,$00,$20
        ;.db $00,$00,$20,$1F,$00,$00,$40,$21,$00,$03,$00,$20,$00,$00,$C0,$3E
        ;.db $40,$1F,$00,$20,$00,$00,$D0,$20,$60,$00,$A0,$1F,$00,$00,$D0,$20
        ;.db $40,$1F,$60,$1F,$00,$00,$60,$20,$40,$1F,$00,$20,$00,$00,$A0,$3F
        ;.db $C0,$00,$00,$20,$A0,$1F,$90,$1F,$00,$C0
        
;****************************
    .sbttl "Maze Arrows"
;****************************       
rtarrow vctr(-18d,4d,hidden)
        vctr(18d,0d,visible)
        vctr(0d,8d,visible)
        vctr(12d,-12d,visible)
        vctr(-12d,-12d,visible)
        vctr(0d,8d,visible)
        vctr(-18d,0d,visible)
        rtsl
        ;.db $17,$42,$29,$40,$20,$44,$26,$5A,$3A,$5A,$20,$44,$37,$40,$00,$C0

uparrow vctr(-4d,-18d,hidden)
        vctr(0d,18d,visible)
        vctr(-8d,0d,visible)
        vctr(12d,12d,visible)
        vctr(12d,-12d,visible)
        vctr(-8d,0d,visible)
        vctr(0d,-18d,visible)
        rtsl
        ;.db $1E,$57,$20,$49,$3C,$40,$26,$46,$26,$5A,$3C,$40,$20,$57,$00,$C0

dnarrow vctr(-4d,18d,hidden)
        vctr(0d,-18d,visible)
        vctr(-8d,0d,visible)
        vctr(12d,-12d,visible)
        vctr(12d,12d,visible)
        vctr(-8d,0d,visible)
        vctr(0d,18d,visible)
        rtsl
        ;.db $1E,$49,$20,$57,$3C,$40,$26,$5A,$26,$46,$3C,$40,$20,$49,$00,$C0

ltarrow vctr(18d,4d,hidden)
        vctr(-18d,0d,visible)
        vctr(0d,8d,visible)
        vctr(-12d,-12d,visible)
        vctr(12d,-12d,visible)
        vctr(0d,8d,visible)
        vctr(18d,0d,visible)
        rtsl
        ;.db $09,$42,$37,$40,$20,$44,$3A,$5A,$26,$5A,$20,$44,$29,$40,$00,$C0

nearrow vctr(-15d,-9d,hidden)
        vctr(12d,12d,visible)
        vctr(-6d,6d,visible)
        vctr(18d,0d,visible)
        vctr(0d,-18d,visible)
        vctr(-6d,6d,visible)
        vctr(-12d,-12d,visible)
        vctr(25d,-1d,hidden)
        rtsl
        ;.db $F7,$1F,$F1,$1F,$26,$46,$3D,$43,$29,$40,$20,$57,$3D,$43,$3A,$5A
        ;.db $FF,$1F,$19,$00,$00,$C0
        
searrow vctr(-15d,9d,hidden)
        vctr(12d,-12d,visible)
        vctr(-6d,-6d,visible)
        vctr(18d,0d,visible)
        vctr(0d,18d,visible)
        vctr(-6d,-6d,visible)
        vctr(-12d,12d,visible)
        rtsl
        ;.db $09,$00,$F1,$1F,$26,$5A,$3D,$5D,$29,$40,$20,$49,$3D,$5D,$3A,$46
        ;.db $00,$C0
        
nwarrow vctr(15d,-9d,hidden)
        vctr(-12d,12d,visible)
        vctr(6d,6d,visible)
        vctr(-18d,0d,visible)
        vctr(0d,-18d,visible)
        vctr(6d,6d,visible)
        vctr(12d,-12d,visible)
        rtsl
        ;.db $F7,$1F,$0F,$00,$3A,$46,$23,$43,$37,$40,$20,$57,$23,$43,$26,$5A
        ;.db $00,$C0
        
swarrow vctr(15d,9d,hidden)
        vctr(-12d,-12d,visible)
        vctr(6d,-6d,visible)
        vctr(-18d,0d,visible)
        vctr(0d,18d,visible)
        vctr(6d,-6d,visible)
        vctr(12d,12d,visible)
        rtsl
        ;.db $09,$00,$0F,$00,$3A,$5A,$23,$5D,$37,$40,$20,$49,$23,$5D,$26,$46
        ;.db $00,$C0
        
;************************************
    .sbttl "Lightning Parts"
;**************************************
ltg7 = $60|page
        
ltg_a   vctr(-24d,-8d,visible)
        vctr(40d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(24d,-8d,visible)
        rtsl
        ;.db $34,$5C,$F8,$1F,$28,$20,$34,$5C,$2C,$5C,$00
        
ltg_b   vctr(8d,-8d,visible)
        vctr(-8d,-8d,visible)
        vctr(16d,-8d,visible)
ltg_b1  vctr(-32d,-8d,visible)
        vctr(24d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(24d,-8d,visible)
        rtsl
        ;.db $24,$5C,$3C,$5C,$28,$5C,$F8,$1F,$E0,$3F,$2C,$5C,$34,$5C,$2C,$5C,$00,$C0

ltg_c   vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
ltg_c1  vctr(-40d,-8d,visible)
        vctr(24d,-8d,visible)
        vctr(-16d,-8d,visible)
        vctr(8d,-8d,visible)
        rtsl        
        ;.db $38,$5C,$F8,$1F,$20,$20,$F8,$1F,$D8,$3F,$2C,$5C,$38,$5C,$24,$5C,$00,$C0
        
ltg_d   vctr(-16d,-8d,visible)
        vctr(32d,-16d,visible)
        vctr(-24d,-8d,visible)
        vctr(24d,-8d,visible)
        rtsl
        ;.db $38,$5C,$F0,$1F,$20,$20,$34,$5C,$2C,$5C,$00,$C0
        
ltg_e   vctr(-40d,-8d,visible)
        vctr(16d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(32d,-8d,visible)
ltg_e1  vctr(-16d,-8d,visible)
        vctr(24d,-8d,visible)
        vctr(-32d,-8d,visible)
        vctr(32d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(8d,-8d,visible)
        rtsl
        ;.db $F8,$1F,$D8,$3F,$28,$5C,$34,$5C,$F8,$1F,$20,$20,$38,$5C
        ;.db $2C,$5C,$F8,$1F,$E0,$3F,$F8,$1F,$20,$20,$34,$5C,$24,$5C
        ;.db $00,$C0
        
;Assembled Lightning (Vertical)

ltng0   vctr(0d,128d,hidden)
        vctr(0d,-256d,visible)
        rtsl
        ;.db $80,$00,$00,$00,$00,$1F,$00,$20,$00,$C0
        
ltng1   vctr(0d,128d,hidden)
        vctr(24d,-8d,visible)
        jsrl(ltg_e) ;$63F6
        vctr(0d,-168d,visible)
        rtsl
        ;.db $80,$00,$00,$00,$2C,$5C,$FB,$B1,$58,$1F,$00,$20,$00,$C0
        
ltng2   vctr(0d,128d,hidden)
        vctr(0d,-16d,visible)
        jsrl(ltg_d)     ;$63EA
        jsrl(ltg_b1)    ;$63CC
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        jsrl(ltg_e)     ;$63F6
        vctr(0d,-72d,visible)
        rtsl
        ;.db $80,$00,$00,$00,$20,$58,$F5,$B1,$E6,$B1,$38,$5C,$F8,$1F,$20,$20
        ;.db $FB,$B1,$B8,$1F,$00,$20,$00,$C0
        
ltng3   vctr(0d,128d,hidden)
        vctr(0d,-24d,visible)
        vctr(8d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(16d,-8d,visible)
        vctr(-16d,-8d,visible)
        vctr(24d,-8d,visible)
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        vctr(-40d,-8d,visible)
        vctr(24d,-8d,visible)
        vctr(-16d,-8d,visible)
        vctr(8d,-8d,visible)
        jsrl(ltg_d)     ;$63EA
        jsrl(ltg_b1)    ;$63CC
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        vctr(-32d,-8d,visible)
        vctr(8d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(32d,-8d,visible)
        vctr(-24d,-8d,visible)
        vctr(8d,-8d,visible)
        vctr(0d,-8d,visible)
        rtsl
        ;.db $80,$00,$00,$00,$20,$54,$24,$5C,$34,$5C,$28,$5C,$38,$5C,$2C,$5C
        ;.db $38,$5C,$F8,$1F,$20,$20,$F8,$1F,$D8,$3F,$2C,$5C,$38,$5C,$24,$5C
        ;.db $F5,$B1,$E6,$B1,$38,$5C,$F8,$1F,$20,$20,$F8,$1F,$E0,$3F,$24,$5C
        ;.db $34,$5C,$F8,$1F,$20,$20,$34,$5C,$24,$5C,$20,$5C,$00,$C0
        
ltng4   vctr(0d,128d,hidden)
        vctr(-32d,-8d,visible)
        vctr(40d,-8d,visible)
        jsrl(ltg_e1)    ;$6402
        vctr(0d,-48d,visible)
        vctr(8d,-8d,visible)
        vctr(-8d,-8d,visible)
        vctr(16d,-8d,visible)
        jsrl(ltg_b1)    ;$63CC
        jsrl(ltg_c)     ;$63D8
        vctr(-16d,-8d,visible)
        vctr(32d,-16d,visible)
        vctr(-24d,-8d,visible)
        vctr(8d,-8d,visible)
        rtsl
        ;.db $80,$00,$00,$00,$F8,$1F,$E0,$3F,$F8,$1F,$28,$20,$01,$B2,$D0,$1F
        ;.db $00,$20,$24,$5C,$3C,$5C,$28,$5C,$E6,$B1,$EC,$B1,$38,$5C,$F0,$1F
        ;.db $20,$20,$34,$5C,$24,$5C,$00,$C0
        
ltng5   vctr(0d,128d,hidden)
        vctr(0d,-8d,visible)
        vctr(-8d,-8d,visible)
        vctr(24d,-8d,visible)
        jsrl(ltg_b1)    ;$63CC
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        jsrl(ltg_e)     ;$63F6
        vctr(0d,-48d,visible)
        jsrl(ltg_b)     ;$63C6
        rtsl
        ;.db $80,$00,$00,$00,$20,$5C,$3C,$5C,$2C,$5C,$E6,$B1,$38,$5C,$F8,$1F
        ;.db $20,$20,$FB,$B1,$D0,$1F,$00,$20,$E3,$B1,$00,$C0
        
ltng6   vctr(0d,128d,hidden)
        vctr(0d,-16d,visible)
        vctr(-16d,-8d,visible)
        vctr(24d,-8d,visible)
        jsrl(ltg_c)     ;$63D8
        jsrl(ltg_a)     ;$63BA
        jsrl(ltg_b1)    ;$63CC
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        jsrl(ltg_e)     ;$63F6
        vctr(0d,-8d,visible)
        rtsl
        ;.db $80,$00,$00,$00,$20,$58,$38,$5C,$2C,$5C,$EC,$B1,$DD,$B1,$E6,$B1
        ;.db $38,$5C,$F8,$1F,$20,$20,$FB,$B1,$20,$5C,$00,$C0
        
ltng7   vctr(0d,128d,hidden)
        vctr(0d,-72d,visible)
        jsrl(ltg_b)     ;$63C6
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        jsrl(ltg_c1)    ;$63DE
        jsrl(ltg_a)     ;$63BA
        jsrl(ltg_b1)    ;$63CC
        vctr(-8d,-8d,visible)
        rtsl
        ;.db $80,$00,$00,$00,$B8,$1F,$00,$20,$E3,$B1,$38,$5C,$F8,$1F,$20,$20
        ;.db $EF,$B1,$DD,$B1,$E6,$B1,$3C,$5C,$00,$C0
        
ltng8   vctr(0d,128d,hidden)
        vctr(0d,-168d,visible)
        jsrl(ltg_b)     ;$63C6
        vctr(-16d,-8d,visible)
        vctr(32d,-8d,visible)
        vctr(-40d,-8d,visible)
        vctr(8d,-8d,visible)
        rtsl
        ;.db $80,$00,$00,$00,$58,$1F,$00,$20,$E3,$B1,$38,$5C,$F8,$1F,$20,$20
        ;.db $F8,$1F,$D8,$3F,$24,$5C,$00,$C0

;Lightning Parts (Horizontal)
        
lng_ax  vctr(-8d,24d,visible)
        vctr(-8d,-40d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-24d,visible)
        rtsl
        ;.db $3C,$4C,$D8,$1F,$F8,$3F,$3C,$4C,$3C,$54,$00,$C0
        
lng_bx  vctr(-8d,-8d,visible)
        vctr(-8d,8d,visible)
        vctr(-8d,-16d,visible)
lng_bx1 vctr(-8d,32d,visible)
        vctr(-8d,-24d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-24d,visible)
        rtsl
        ;.db $3C,$5C,$3C,$44,$3C,$58,$20,$00,$F8,$3F,$3C,$54,$3C,$4C,$3C,$54
        ;.db $00,$C0
 
lng_cx  vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
lng_cx1 vctr(-8d,40d,visible)
        vctr(-8d,-24d,visible)
        vctr(-8d,16d,visible)
        vctr(-8d,-8d,visible)
        rtsl
        ;.db $3C,$48,$E0,$1F,$F8,$3F,$28,$00,$F8,$3F,$3C,$54,$3C,$48,$3C,$5C
        ;.db $00,$C0
        
lng_dx  vctr(-8d,16d,visible)
        vctr(-16d,-32d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-24d,visible)
        rtsl
        ;.db $3C,$48,$E0,$1F,$F0,$3F,$3C,$4C,$3C,$54,$00,$C0
        
lng_ex  vctr(-8d,40d,visible)
        vctr(-8d,-16d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-32d,visible)
lng_ex1 vctr(-8d,16d,visible)
        vctr(-8d,-24d,visible)
        vctr(-8d,32d,visible)
        vctr(-8d,-32d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-8d,visible)
        rtsl
        ;.db $28,$00,$F8,$3F,$3C,$58,$3C,$4C,$E0,$1F,$F8,$3F,$3C,$48,$3C,$54
        ;.db $20,$00,$F8,$3F,$E0,$1F,$F8,$3F,$3C,$4C,$3C,$5C,$00,$C0

;Assembled Lightning (Horizontal)
        
ltng0x  vctr(128d,0d,hidden)
        vctr(-256d,0d,visible)
        rtsl
        ;.db $00,$00,$80,$00,$00,$00,$00,$3F,$00,$C0
    
ltng1x  vctr(128d,0d,hidden)
        vctr(-8d,-24d,visible)
        jsrl(lng_ex)    ;$6550
        vctr(-168d,0d,visible)
        rtsl
        ;.db $00,$00,$80,$00,$3C,$54,$A8,$B2,$00,$00,$58,$3F,$00,$C0
        
ltng2x  vctr(128d,0d,hidden)
        vctr(-16d,0d,visible)
        jsrl(lng_dx)    ;$6544
        jsrl(lng_bx1)   ;$6526
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        jsrl(lng_ex)    ;$6550
        vctr(-72d,0d,visible)
        rtsl
        ;.db $00,$00,$80,$00,$38,$40,$A2,$B2,$93,$B2,$3C,$48,$E0,$1F,$F8,$3F
        ;.db $A8,$B2,$00,$00,$B8,$3F,$00,$C0
        
ltng3x  vctr(128d,0d,hidden)
        vctr(-24d,0d,visible)
        vctr(-8d,-8d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-16d,visible)
        vctr(-8d,16d,visible)
        vctr(-8d,-24d,visible)
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        vctr(-8d,40d,visible)
        vctr(-8d,-24d,visible)
        vctr(-8d,16d,visible)
        vctr(-8d,-8d,visible)
        jsrl(lng_dx)    ;$6544
        jsrl(lng_bx1)   ;$6526
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        vctr(-8d,32d,visible)
        vctr(-8d,-8d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-32d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-8d,visible)
        vctr(-8d,0d,visible)
        rtsl
        ;.db $00,$00,$80,$00,$34,$40,$3C,$5C,$3C,$4C,$3C,$58,$3C,$48,$3C,$54
        ;.db $3C,$48,$E0,$1F,$F8,$3F,$28,$00,$F8,$3F,$3C,$54,$3C,$48,$3C,$5C
        ;.db $A2,$B2,$93,$B2,$3C,$48,$E0,$1F,$F8,$3F,$20,$00,$F8,$3F,$3C,$5C
        ;.db $3C,$4C,$E0,$1F,$F8,$3F,$3C,$4C,$3C,$5C,$3C,$40,$00,$C0
        
ltng4x  vctr(128d,0d,hidden)
        vctr(-8d,32d,visible)
        vctr(-8d,-40d,visible)
        jsrl(lng_ex1)   ;$655C
        vctr(-48d,0d,visible)
        vctr(-8d,-8d,visible)
        vctr(-8d,8d,visible)
        vctr(-8d,-16d,visible)
        jsrl(lng_bx1)   ;$6526
        jsrl(lng_cx)    ;$6532
        vctr(-8d,16d,visible)
        vctr(-16d,-32d,visible)
        vctr(-8d,24d,visible)
        vctr(-8d,-8d,visible)
        rtsl
        ;.db $00,$00,$80,$00,$20,$00,$F8,$3F,$D8,$1F,$F8,$3F,$AE,$B2,$00,$00
        ;.db $D0,$3F,$3C,$5C,$3C,$44,$3C,$58,$93,$B2,$99,$B2,$3C,$48,$E0,$1F
        ;.db $F0,$3F,$3C,$4C,$3C,$5C,$00,$C0
        
ltng5x  vctr(128d,0d,hidden)
        vctr(-8d,0d,visible)
        vctr(-8d,8d,visible)
        vctr(-8d,-24d,visible)
        jsrl(lng_bx1)   ;$6526
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        jsrl(lng_ex)    ;$6550
        vctr(-48d,0d,visible)
        jsrl(lng_bx)    ;$6520
        rtsl
        ;.db $00,$00,$80,$00,$3C,$40,$3C,$44,$3C,$54,$93,$B2,$3C,$48,$E0,$1F
        ;.db $F8,$3F,$A8,$B2,$00,$00,$D0,$3F,$90,$B2,$00,$C0
        
ltng6x  vctr(128d,0d,hidden)
        vctr(-16d,0d,visible)
        vctr(-8d,16d,visible)
        vctr(-8d,-24d,visible)
        jsrl(lng_cx)    ;$6532
        jsrl(lng_ax)    ;$6514
        jsrl(lng_bx1)   ;$6526
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        jsrl(lng_ex)    ;$6550
        vctr(-8d,0d,visible)
        rtsl
        ;.db $00,$00,$80,$00,$38,$40,$3C,$48,$3C,$54,$99,$B2,$8A,$B2,$93,$B2
        ;.db $3C,$48,$E0,$1F,$F8,$3F,$A8,$B2,$3C,$40,$00,$C0
        
ltng7x  vctr(128d,0d,hidden)
        vctr(-72d,0d,visible)
        jsrl(lng_bx)    ;$6520
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        jsrl(lng_cx1)   ;$6538
        jsrl(lng_ax)    ;$6514
        jsrl(lng_bx1)   ;$6526
        vctr(-8d,8d,visible)
        vctr(256d,0d,hidden)
        rtsl
        ;.db $00,$00,$80,$00,$00,$00,$B8,$3F,$90,$B2,$3C,$48,$E0,$1F,$F8,$3F
        ;.db $9C,$B2,$8A,$B2,$93,$B2,$3C,$44,$00,$00,$00,$01,$00,$C0
        
ltng8x  vctr(128d,0d,hidden)
        vctr(-168d,0d,visible)
        jsrl(lng_bx)    ;$6520
        vctr(-8d,16d,visible)
        vctr(-8d,-32d,visible)
        vctr(-8d,40d,visible)
        vctr(-8d,-8d,visible)
        rtsl
        ;.db $00,$00,$80,$00,$00,$00,$58,$3F,$90,$B2,$3C,$48,$E0,$1F,$F8,$3F
        ;.db $28,$00,$F8,$3F,$3C,$5C,$00,$C0
        
        
        
dod0    vctr(91d,137d,hidden)
        vctr(-101d,21d,visible)
        vctr(-88d,-28d,visible)
        vctr(-60d,-89d,visible)
        vctr(10d,-93d,visible)
        vctr(57d,-85d,visible)
        vctr(101d,-21d,visible)
        vctr(88d,28d,visible)
        vctr(60d,89d,visible)
        vctr(-10d,93d,visible)
        vctr(-57d,85d,visible)
        vctr(-24d,-39d,visible)
        vctr(41d,-111d,visible)
        vctr(50d,-28d,visible)
        vctr(-50d,28d,hidden)
        vctr(-91d,-71d,visible)
        vctr(-7d,-74d,visible)
        vctr(7d,74d,hidden)
        vctr(-98d,66d,visible)
        vctr(-67d,-34d,visible)
        vctr(67d,34d,hidden)
        vctr(31d,111d,visible)
        vctr(-48d,37d,visible)
        vctr(48d,-37d,hidden)
        vctr(117d,5d,visible)
        vctr(-67d,-98d,hidden)
        rtsl
        ;.db $89,$00,$5B,$00,$15,$00,$9B,$3F,$E4,$1F,$A8,$3F,$A7,$1F,$C4,$3F
        ;.db $A3,$1F,$0A,$20,$AB,$1F,$39,$20,$EB,$1F,$65,$20,$1C,$00,$58,$20
        ;.db $59,$00,$3C,$20,$5D,$00,$F6,$3F,$55,$00,$C7,$3F,$D9,$1F,$E8,$3F
        ;.db $91,$1F,$29,$20,$E4,$1F,$32,$20,$1C,$00,$CE,$1F,$B9,$1F,$A5,$3F
        ;.db $B6,$1F,$F9,$3F,$4A,$00,$07,$00,$42,$00,$9E,$3F,$DE,$1F,$BD,$3F
        ;.db $22,$00,$43,$00,$6F,$00,$1F,$20,$25,$00,$D0,$3F,$DB,$1F,$30,$00
        ;.db $05,$00,$75,$20,$9E,$1F,$BD,$1F,$00,$C0
        
dod1    vctr(91d,110d,hidden)
        vctr(-24d,38d,visible)
        vctr(-117d,5d,visible)
        vctr(-48d,-37d,visible)
        vctr(-61d,-98d,visible)
        vctr(50d,-88d,visible)
        vctr(42d,-78d,visible)
        vctr(117d,-5d,visible)
        vctr(48d,37d,visible)
        vctr(61d,98d,visible)
        vctr(-11d,24d,visible)
        vctr(-57d,104d,visible)
        vctr(-102d,-21d,visible)
        vctr(-6d,-115d,visible)
        vctr(98d,-51d,visible)
        vctr(67d,83d,visible)
        vctr(-67d,-83d,hidden)
        vctr(-31d,-76d,visible)
        vctr(-67d,127d,hidden)
        vctr(-92d,-44d,visible)
        vctr(98d,159d,hidden)
        vctr(-87d,27d,visible)
        vctr(98d,-116d,hidden)
        rtsl
        ;.db $6E,$00,$5B,$00,$26,$00,$E8,$3F,$05,$00,$8B,$3F,$DB,$1F,$D0,$3F
        ;.db $9E,$1F,$C3,$3F,$A8,$1F,$32,$20,$B2,$1F,$2A,$20,$FB,$1F,$75,$20
        ;.db $25,$00,$30,$20,$62,$00,$3D,$20,$18,$00,$F5,$3F,$68,$00,$C7,$3F
        ;.db $EB,$1F,$9A,$3F,$8D,$1F,$FA,$3F,$CD,$1F,$62,$20,$53,$00,$43,$20
        ;.db $AD,$1F,$BD,$1F,$B4,$1F,$E1,$3F,$7F,$00,$BD,$1F,$D4,$1F,$A4,$3F
        ;.db $9F,$00,$62,$00,$1B,$00,$A9,$3F,$8C,$1F,$62,$00,$00,$C0
        
dod2    vctr(100d,123d,hidden)
        vctr(-69d,33d,visible)
        vctr(-112d,-13d,visible)
        vctr(-50d,-81d,visible)
        vctr(-31d,-91d,visible)
        vctr(62d,-94d,visible)
        vctr(69d,-33d,visible)
        vctr(112d,13d,visible)
        vctr(50d,81d,visible)
        vctr(31d,91d,visible)
        vctr(-62d,94d,visible)
        vctr(-69d,-32d,visible)
        vctr(19d,-115d,visible)
        vctr(81d,-38d,visible)
        vctr(-81d,38d,hidden)
        vctr(-100d,-58d,visible)
        vctr(19d,-74d,visible)
        vctr(-19d,74d,hidden)
        vctr(-81d,79d,visible)
        vctr(-31d,-26d,visible)
        vctr(31d,26d,hidden)
        vctr(50d,106d,visible)
        vctr(0d,40d,visible)
        vctr(0d,-40d,hidden)
        vctr(112d,-12d,visible)
        vctr(-31d,-91d,hidden)
        rtsl
        ;.db $7B,$00,$64,$00,$21,$00,$BB,$3F,$F3,$1F,$90,$3F,$AF,$1F,$CE,$3F
        ;.db $A5,$1F,$E1,$3F,$A2,$1F,$3E,$20,$DF,$1F,$45,$20,$0D,$00,$70,$20
        ;.db $51,$00,$32,$20,$5B,$00,$1F,$20,$5E,$00,$C2,$3F,$E0,$1F,$BB,$3F
        ;.db $8D,$1F,$13,$20,$DA,$1F,$51,$20,$26,$00,$AF,$1F,$C6,$1F,$9C,$3F
        ;.db $B6,$1F,$13,$20,$4A,$00,$ED,$1F,$4F,$00,$AF,$3F,$E6,$1F,$E1,$3F
        ;.db $1A,$00,$1F,$00,$6A,$00,$32,$20,$28,$00,$00,$20,$D8,$1F,$00,$00
        ;.db $F4,$1F,$70,$20,$A5,$1F,$E1,$1F,$00,$C0
        
pupl00  vctr(-32d,-4d,hidden)
        jsrl(puple01)   ;$67B4
        jsrl(puple00)   ;$67B0
        vctr(-4d,0d,hidden)
        rtsl
        ;.db $FC,$1F,$E0,$1F,$DA,$B3,$D8,$B3,$1E,$40,$00,$C0

puple00 vctr(36d,4d,hidden)   ;7B0
puple01 vctr(8d,14d,visible)
        vctr(8d,-10d,visible)
        vctr(-8d,-14d,visible)
        vctr(-8d,10d,visible)
        rtsl
        ;.db $04,$00,$24,$00,$24,$47,$24,$5B,$3C,$59,$3C,$45,$00,$C0
        
puple02 vctr(18d,26d,visible) ;7BE
        vctr(18d,-22d,visible)
        vctr(-18d,-28d,visible)
        vctr(-36d,48d,visible)
        vctr(-16d,-28d,visible)
        vctr(16d,-24d,visible)
        vctr(18d,28d,visible)
        vctr(8d,14d,hidden)
        vctr(-8d,6d,visible)
        vctr(-60d,26d,visible)
        rtsl
        ;.db $29,$4D,$29,$55,$37,$52,$30,$00,$DC,$3F,$38,$52,$28,$54
        ;.db $29,$4E,$04,$47,$3C,$43,$1A,$00,$C4,$3F,$00,$C0
        
pupc0   vctr(40d,0d,visible)  ;7D8
        vctr(20d,22d,visible)
        vctr(0d,30d,visible)
        vctr(-6d,4d,visible)
        rtsl
        ;.db $00,$00,$28,$20,$2A,$4B,$20,$4F,$3D,$42,$00,$C0

wing00  jsrl(puple02)   ;$67BE
        vctr(8d,-50d,visible)
        vctr(8d,-2d,hidden)
        vctr(-68d,14d,visible)
        vctr(76d,-44d,visible)
        vctr(-8d,6d,hidden)
        vctr(0d,-2d,visible)
        vctr(24d,-28d,visible)
        jsrl(pupc0)     ;$67D8
        vctr(0d,2d,hidden)
        vctr(68d,18d,visible)
        vctr(-62d,-48d,visible)
        vctr(-38d,32d,hidden)
        rtsl
        ;.db $DF,$B3,$CE,$1F,$08,$20,$04,$5F,$0E,$00,$BC,$3F,$D4,$1F,$4C,$20
        ;.db $1C,$43,$20,$5F,$2C,$52,$EC,$B3,$00,$41,$12,$00,$44,$20,$D0,$1F
        ;.db $C2,$3F,$20,$00,$DA,$1F,$00,$C0
        
wing01  jsrl(puple02)   ;$67BE
        vctr(8d,-52d,visible)
        vctr(8d,0d,hidden)
        vctr(-66d,-8d,visible)
        vctr(74d,-22d,visible)
        vctr(-10d,4d,hidden)
        vctr(24d,-28d,visible)
        jsrl(pupc0)     ;$67D8
        vctr(70d,0d,visible)
        vctr(-64d,-28d,visible)
        vctr(-38d,32d,hidden)
        rtsl
        ;.db $DF,$B3,$CC,$1F,$08,$20,$04,$40,$F8,$1F,$BE,$3F,$EA,$1F,$4A,$20
        ;.db $1B,$42,$2C,$52,$EC,$B3,$00,$00,$46,$20,$E4,$1F,$C0,$3F,$20,$00
        ;.db $DA,$1F,$00,$C0
        
wing02  jsrl(puple02)   ;$67BE
        vctr(10d,-54d,visible)
        vctr(6d,2d,hidden)
        vctr(-66d,-28d,visible)
        vctr(74d,-2d,visible)
        vctr(-8d,0d,hidden)
        vctr(22d,-24d,visible)
        jsrl(pupc0)     ;$67D8
        vctr(6d,-4d,hidden)
        vctr(64d,-16d,visible)
        vctr(-64d,-10d,visible)
        vctr(-38d,34d,hidden)
        rtsl
        ;.db $DF,$B3,$CA,$1F,$0A,$20,$03,$41,$E4,$1F,$BE,$3F,$FE,$1F,$4A,$20
        ;.db $1C,$40,$2B,$54,$EC,$B3,$03,$5E,$F0,$1F,$40,$20,$F6,$1F,$C0,$3F
        ;.db $22,$00,$DA,$1F,$00,$C0
        
wing03  jsrl(puple02)   ;$67BE
        vctr(10d,-58d,visible)
        vctr(6d,6d,hidden)
        vctr(-66d,-56d,visible)
        vctr(74d,26d,visible)
        vctr(-6d,-2d,hidden)
        vctr(20d,-22d,visible)
        jsrl(pupc0)     ;$67D8
        vctr(6d,-6d,hidden)
        vctr(64d,-34d,visible)
        vctr(-64d,10d,visible)
        vctr(-38d,34d,hidden)
        rtsl
        ;.db $DF,$B3,$C6,$1F,$0A,$20,$03,$43,$C8,$1F,$BE,$3F,$1A,$00,$4A,$20
        ;.db $1D,$5F,$2A,$55,$EC,$B3,$03,$5D,$DE,$1F,$40,$20,$0A,$00,$C0,$3F
        ;.db $22,$00,$DA,$1F,$00,$C0
        
pupl10  vctr(-22d,-2d,hidden)
        jsrl(puple1_)   ;$6888
        jsrl(puple10)   ;$6886
        vctr(-2d,-6d,hidden)
        rtsl
        ;.db $15,$5F,$44,$B4,$43,$B4,$1F,$5D,$00,$C0
        
puple10 vctr(24d,8d,hidden)       ;886
puple1_ vctr(8d,14d,visible)
        vctr(8d,-12d,visible)
        vctr(-8d,-14d,visible)
        vctr(-8d,12d,visible)
        rtsl
        ;.db $0C,$44,$24,$47,$24,$5A,$3C,$59,$3C,$46 
        ;.db $00,$C0
        
puple11 vctr(-14d,-26d,visible)   ;892
        vctr(-18d,24d,visible)
        vctr(18d,26d,visible)
        vctr(26d,-42d,visible)
        vctr(16d,26d,visible)
        vctr(-16d,24d,visible)
        vctr(-16d,-22d,visible)
        vctr(-12d,8d,hidden)
        vctr(-8d,2d,visible)
        vctr(-94d,-2d,visible)
        rtsl
        ;.db $39,$53,$37,$4C,$29,$4D,$D6,$1F,$1A,$20,$28,$4D 
        ;.db $38,$4C,$38,$55,$1A,$44,$3C,$41,$FE,$1F,$A2,$3F
        ;.db $00,$C0
        
puple12 vctr(34d,6d,visible)  ;8AC
        vctr(12d,22d,visible)
        vctr(0d,28d,visible)
        vctr(-12d,6d,visible)
        rtsl
        ;.db $06,$00,$22,$20,$26,$4B,$20,$4E,$3A,$43,$00,$C0
        
wing10  jsrl(puple11)   ;$6892
        vctr(36d,-32d,visible)
        vctr(30d,0d,hidden)
        vctr(-60d,0d,visible)
        vctr(88d,-26d,visible)
        vctr(-42d,12d,hidden)
        vctr(22d,-18d,visible)
        vctr(34d,-16d,visible)
        jsrl(puple12)   ;$68AC
        vctr(-2d,16d,hidden)
        vctr(50d,18d,visible)
        vctr(-36d,-48d,visible)
        vctr(-34d,14d,hidden)
        rtsl
        ;.db $49,$B4,$E0,$1F,$24,$20,$0F,$40,$00,$00,$C4,$3F,$E6,$1F,$58,$20
        ;.db $0C,$00,$D6,$1F,$2B,$57,$F0,$1F,$22,$20,$56,$B4,$1F,$48,$12,$00
        ;.db $32,$20,$D0,$1F,$DC,$3F,$0E,$00,$DE,$1F,$00,$C0
        
wing11  jsrl(puple11)   ;$6892
        vctr(46d,-40d,visible)
        vctr(20d,8d,hidden)
        vctr(-58d,-20d,visible)
        vctr(86d,-6d,visible)
        vctr(-30d,2d,hidden)
        vctr(10d,-8d,visible)
        vctr(34d,-16d,visible)
        jsrl(puple12)   ;$68AC
        vctr(4d,8d,hidden)
        vctr(50d,10d,visible)
        vctr(-42d,-36d,visible)
        vctr(-34d,18d,hidden)
        rtsl
        ;.db $49,$B4,$D8,$1F,$2E,$20,$0A,$44,$EC,$1F,$C6,$3F,$FA,$1F,$56,$20
        ;.db $11,$41,$25,$5C,$F0,$1F,$22,$20,$56,$B4,$02,$44,$0A,$00,$32,$20
        ;.db $DC,$1F,$D6,$3F,$12,$00,$DE,$1F,$00,$C0
        
wing12  jsrl(puple11)   ;$6892
        vctr(50d,-44d,visible)
        vctr(16d,12d,hidden)
        vctr(-54d,-40d,visible)
        vctr(82d,14d,visible)
        vctr(-24d,-4d,hidden)
        vctr(4d,-2d,visible)
        vctr(34d,-16d,visible)
        jsrl(puple12)   ;$68AC
        vctr(52d,0d,visible)
        vctr(-46d,-24d,visible)
        vctr(-34d,24d,hidden)
        rtsl
        ;.db $49,$B4,$D4,$1F,$32,$20,$08,$46,$D8,$1F,$CA,$3F,$0E,$00,$52,$20
        ;.db $14,$5E,$22,$5F,$F0,$1F,$22,$20,$56,$B4,$00,$00,$34,$20,$E8,$1F
        ;.db $D2,$3F,$18,$00,$DE,$1F,$00,$C0
        
wing13  jsrl(puple11)   ;$6892
        vctr(52d,-46d,visible)
        vctr(14d,14d,hidden)
        vctr(-52d,-58d,visible)
        vctr(80d,32d,visible)
        vctr(-18d,-8d,hidden)
        vctr(32d,-14d,visible)
        jsrl(puple12)   ;$68AC
        vctr(12d,-6d,hidden)
        vctr(50d,-10d,visible)
        vctr(-50d,-12d,visible)
        vctr(-34d,28d,hidden)
        rtsl
        ;.db $49,$B4,$D2,$1F,$34,$20,$07,$47,$C6,$1F,$CC,$3F,$20,$00,$50,$20
        ;.db $17,$5C,$F2,$1F,$20,$20,$56,$B4,$06,$5D,$F6,$1F,$32,$20,$F4,$1F
        ;.db $CE,$3F,$1C,$00,$DE,$1F,$00,$C0
        
pupl20  vctr(-8d,-12d,hidden)
        vctr(-8d,12d,visible)
        vctr(8d,12d,visible)
        vctr(10d,-8d,hidden)
        vctr(-8d,14d,visible)
        vctr(6d,8d,visible)
        vctr(0d,-26d,hidden)
        rtsl
        ;.db $1C,$5A,$3C,$46,$24,$46,$05,$5C,$3C,$47,$23,$44,$00,$53,$00,$C0
        
pupl21  vctr(-16d,-24d,visible)
        vctr(-18d,24d,visible)
        vctr(26d,36d,visible)
        vctr(16d,-22d,visible)
        vctr(-10d,-14d,visible)
        vctr(-16d,24d,visible)
        vctr(-10d,-8d,hidden)
        vctr(-16d,4d,visible)
        vctr(-126d,-28d,visible)
        rtsl
        ;.db $38,$54,$37,$4C,$24,$00,$1A,$20,$28,$55,$3B,$59,$38,$4C,$1B,$5C
        ;.db $38,$42,$E4,$1F,$82,$3F,$00,$C0
        
pupl22  vctr(46d,2d,visible)
        vctr(20d,26d,visible)
        vctr(-10d,34d,visible)
        vctr(-10d,2d,visible)
        rtsl
        ;.db $02,$00,$2E,$20,$2A,$4D,$22,$00,$F6,$3F,$3B,$41,$00,$C0
        
wing20  jsrl(pupl21)    ;$696E
        vctr(124d,-50d,visible)
        vctr(-16d,42d,hidden)
        vctr(-14d,-28d,visible)
        vctr(52d,8d,visible)
        vctr(-22d,-22d,hidden)
        jsrl(pupl22)    ;$6986
        vctr(-34d,12d,hidden)
        vctr(-34d,38d,visible)
        vctr(0d,-30d,visible)
        vctr(-4d,-24d,hidden)
        rtsl
        ;.db $B7,$B4,$CE,$1F,$7C,$20,$2A,$00,$F0,$1F,$39,$52,$08,$00,$34,$20
        ;.db $15,$55,$C3,$B4,$0C,$00,$DE,$1F,$26,$00,$DE,$3F,$20,$51,$1E,$54
        ;.db $00,$C0
        
wing21  jsrl(pupl21)    ;$696E
        vctr(98d,-40d,visible)
        vctr(10d,32d,hidden)
        vctr(-14d,-42d,visible)
        vctr(52d,22d,visible)
        vctr(-36d,-16d,hidden)
        vctr(14d,-6d,visible)
        jsrl(pupl22)    ;$6986
        vctr(-34d,12d,hidden)
        vctr(14d,8d,visible)
        vctr(12d,8d,hidden)
        vctr(16d,10d,visible)
        vctr(-4d,-24d,visible)
        vctr(-8d,-18d,hidden)
        rtsl
        ;.db $B7,$B4,$D8,$1F,$62,$20,$20,$00,$0A,$00,$D6,$1F,$F2,$3F,$16,$00
        ;.db $34,$20,$F0,$1F,$DC,$1F,$27,$5D,$C3,$B4,$0C,$00,$DE,$1F,$27,$44
        ;.db $06,$44,$28,$45,$3E,$54,$1C,$57,$00,$C0
        
wing22  jsrl(pupl21)    ;$696E
        vctr(100d,-40d,visible)
        vctr(8d,32d,hidden)
        vctr(-14d,-58d,visible)
        vctr(52d,38d,visible)
        vctr(-28d,-20d,hidden)
        vctr(6d,-2d,visible)
        jsrl(pupl22)    ;$6986
        vctr(0d,20d,hidden)
        vctr(14d,4d,visible)
        vctr(-10d,-24d,visible)
        vctr(-8d,-4d,hidden)
        rtsl
        ;.db $B7,$B4,$D8,$1F,$64,$20,$20,$00,$08,$00,$C6,$1F,$F2,$3F,$26,$00
        ;.db $34,$20,$12,$56,$23,$5F,$C3,$B4,$00,$4A,$27,$42,$3B,$54,$1C,$5E
        ;.db $00,$C0
        
wing23  jsrl(pupl21)    ;$696E
        vctr(102d,-42d,visible)
        vctr(6d,34d,hidden)
        vctr(-14d,-72d,visible)
        vctr(52d,52d,visible)
        vctr(-22d,-22d,hidden)
        jsrl(pupl22)    ;$6986
        vctr(6d,12d,hidden)
        vctr(14d,2d,visible)
        vctr(-12d,-16d,visible)
        vctr(-12d,-2d,hidden)
        rtsl
        ;.db $B7,$B4,$D6,$1F,$66,$20,$22,$00,$06,$00,$B8,$1F,$F2,$3F,$34,$00
        ;.db $34,$20,$15,$55,$C3,$B4,$03,$46,$27,$41,$3A,$58,$1A,$5F,$00,$C0

pupl30  vctr(26d,-12d,hidden)
        vctr(-8d,12d,visible)
        vctr(8d,12d,visible)
        vctr(-26d,-12d,hidden)
        rtsl
        ;.db $0D,$5A,$3C,$46,$24,$46,$13,$5A,$00,$C0
        
        
pupl31  vctr(18d,24d,visible)
        vctr(16d,-24d,visible)
        vctr(-18d,-26d,visible)
        vctr(-16d,26d,visible)
        vctr(-6d,14d,hidden)
        vctr(16d,18d,visible)
        vctr(6d,-10d,visible)
        vctr(-8d,-10d,hidden)
        vctr(-36d,2d,visible)
        vctr(-112d,-52d,visible)
        vctr(134d,-22d,visible)
        rtsl
        ;.db $29,$4C,$28,$54,$37,$53,$38,$4D,$1D,$47,$28,$49,$23,$5B,$1C,$5B
        ;.db $02,$00,$DC,$3F,$CC,$1F,$90,$3F,$EA,$1F,$86,$20,$00,$C0
        
pupl32  vctr(12d,34d,visible)
        vctr(-16d,26d,visible)
        vctr(-4d,0d,visible)
        rtsl
        ;.db $22,$00,$0C,$20,$38,$4D,$3E,$40,$00,$C0
        
wing33  jsrl(pupl31)    ;$6A2C
        vctr(-8d,34d,hidden)
        vctr(18d,-70d,visible)
        vctr(22d,66d,visible)
        vctr(-8d,-24d,hidden)
        vctr(18d,4d,visible)
        jsrl(pupl32)    ;$6A4A
        vctr(-62d,2d,hidden)
        vctr(0d,4d,visible)
        vctr(18d,-2d,visible)
        vctr(16d,-14d,hidden)
        rtsl
        ;.db $16,$B5,$22,$00,$F8,$1F,$BA,$1F,$12,$20,$42,$00,$16,$20,$1C,$54
        ;.db $29,$42,$25,$B5,$02,$00,$C2,$1F,$20,$42,$29,$5F,$08,$59,$00,$C0

wing30  jsrl(pupl31)    ;$6A2C
        vctr(-8d,34d,hidden)
        vctr(46d,-30d,visible)
        vctr(-6d,26d,visible)
        vctr(6d,-22d,hidden)
        vctr(4d,2d,visible)
        jsrl(pupl32)    ;$6A4A
        vctr(-54d,4d,hidden)
        vctr(8d,38d,visible)
        vctr(16d,-34d,visible)
        vctr(2d,-18d,hidden)
        rtsl
        ;.db $16,$B5,$22,$00,$F8,$1F,$E2,$1F,$2E,$20,$3D,$4D,$03,$55,$22,$41
        ;.db $25,$B5,$04,$00,$CA,$1F,$26,$00,$08,$20,$DE,$1F,$10,$20,$01,$57
        ;.db $00,$C0
        
wing31  jsrl(pupl31)    ;$6A2C
        vctr(-8d,34d,hidden)
        vctr(36d,-42d,visible)
        vctr(4d,38d,visible)
        vctr(-2d,-24d,hidden)
        vctr(12d,4d,visible)
        jsrl(pupl32)    ;$6A4A
        vctr(-56d,4d,hidden)
        vctr(4d,26d,visible)
        vctr(20d,-24d,visible)
        vctr(4d,-16d,hidden)
        rtsl
        ;.db $16,$B5,$22,$00,$F8,$1F,$D6,$1F,$24,$20,$26,$00,$04,$20,$1F,$54
        ;.db $26,$42,$25,$B5,$04,$00,$C8,$1F,$22,$4D,$2A,$54,$02,$58,$00,$C0

wing32  jsrl(pupl31)    ;$6A2C
        vctr(-8d,34d,hidden)
        vctr(28d,-56d,visible)
        vctr(12d,52d,visible)
        vctr(-6d,-24d,hidden)
        vctr(16d,4d,visible)
        jsrl(pupl32)    ;$6A4A
        vctr(-58d,2d,hidden)
        vctr(0d,16d,visible)
        vctr(24d,-14d,visible)
        vctr(6d,-14d,hidden)
        rtsl
        ;.db $16,$B5,$22,$00,$F8,$1F,$C8,$1F,$1C,$20,$34,$00,$0C,$20,$1D,$54
        ;.db $28,$42,$25,$B5,$02,$00,$C6,$1F,$20,$48,$2C,$59,$03,$59,$00,$C0

pupl40  vctr(26d,-12d,hidden)     ;AD6
        vctr(-8d,12d,visible)
        vctr(8d,12d,visible)
        vctr(-26d,-8d,hidden)
        vctr(-2d,6d,visible)
        vctr(6d,8d,visible)
        vctr(-2d,-18d,hidden)
        rtsl
        ;.db $0D,$5A,$3C,$46,$24,$46,$13,$5C,$3F,$43,$23,$44,$1F,$57,$00,$C0


pupl41  vctr(18d,24d,visible)     ;AE6
        vctr(16d,-22d,visible)
        vctr(-16d,-26d,visible)
        vctr(-18d,24d,visible)
        vctr(8d,10d,hidden)
        vctr(-14d,22d,visible)
        vctr(-16d,-22d,visible)
        vctr(8d,-10d,visible)
        vctr(18d,4d,hidden)
        vctr(-32d,-8d,visible)
        vctr(-82d,-72d,visible)
        vctr(120d,8d,visible)
        rtsl
        ;.db $29,$4C,$28,$55,$38,$53,$37,$4C,$04,$45,$39,$4B,$38,$55,$24,$5B
        ;.db $09,$42,$F8,$1F,$E0,$3F,$B8,$1F,$AE,$3F,$08,$00,$78,$20,$00,$C0

wing43  jsrl(pupl41)    ;$6AE6
        vctr(18d,10d,visible)
        vctr(-26d,28d,hidden)
        vctr(60d,-62d,visible)
        vctr(-24d,52d,visible)
        vctr(4d,-10d,hidden)
        vctr(2d,2d,visible)
        vctr(2d,34d,visible)
        vctr(-12d,14d,visible)
        vctr(-88d,-26d,hidden)
        vctr(-16d,4d,visible)
        vctr(36d,14d,visible)
        vctr(34d,8d,hidden)
        rtsl
        ;.db $73,$B5,$29,$45,$13,$4E,$C2,$1F,$3C,$20,$34,$00,$E8,$3F,$02,$5B
        ;.db $21,$41,$22,$00,$02,$20,$3A,$47,$E6,$1F,$A8,$1F,$38,$42,$0E,$00
        ;.db $24,$20,$08,$00,$22,$00,$00,$C0
        
wing42  jsrl(pupl41)    ;$6AE6
        vctr(28d,16d,visible)
        vctr(-36d,22d,hidden)
        vctr(68d,-38d,visible)
        vctr(-32d,28d,visible)
        vctr(6d,-6d,hidden)
        vctr(2d,32d,visible)
        vctr(-12d,14d,visible)
        vctr(-82d,-20d,hidden)
        vctr(-20d,18d,visible)
        vctr(46d,0d,visible)
        vctr(22d,2d,hidden)
        rtsl
        ;.db $73,$B5,$2E,$48,$16,$00,$DC,$1F,$DA,$1F,$44,$20,$1C,$00,$E0,$3F
        ;.db $03,$5D,$20,$00,$02,$20,$3A,$47,$EC,$1F,$AE,$1F,$36,$49,$00,$00
        ;.db $2E,$20,$0B,$41,$00,$C0
        
wing41  jsrl(pupl41)    ;$6AE6
        vctr(34d,20d,visible)
        vctr(0d,6d,visible)
        vctr(-42d,12d,hidden)
        vctr(78d,-16d,visible)
        vctr(-42d,6d,visible)
        vctr(6d,2d,hidden)
        vctr(2d,24d,visible)
        vctr(-12d,14d,visible)
        vctr(-76d,-16d,hidden)
        vctr(-22d,32d,visible)
        vctr(44d,-18d,visible)
        vctr(20d,2d,hidden)
        rtsl
        ;.db $73,$B5,$14,$00,$22,$20,$20,$43,$0C,$00,$D6,$1F,$F0,$1F,$4E,$20
        ;.db $06,$00,$D6,$3F,$03,$41,$21,$4C,$3A,$47,$F0,$1F,$B4,$1F,$20,$00
        ;.db $EA,$3F,$EE,$1F,$2C,$20,$0A,$41,$00,$C0
        
wing40  jsrl(pupl41)    ;$6AE6
        vctr(34d,20d,visible)
        vctr(0d,10d,visible)
        vctr(-42d,8d,hidden)
        vctr(86d,8d,visible)
        vctr(-50d,-18d,visible)
        vctr(8d,14d,hidden)
        vctr(0d,12d,visible)
        vctr(-12d,14d,visible)
        vctr(-72d,-12d,hidden)
        vctr(-24d,50d,visible)
        vctr(46d,-38d,visible)
        vctr(16d,0d,hidden)
        rtsl
        ;.db $73,$B5,$14,$00,$22,$20,$20,$45,$08,$00,$D6,$1F,$08,$00,$56,$20
        ;.db $EE,$1F,$CE,$3F,$04,$47,$20,$46,$3A,$47,$F4,$1F,$B8,$1F,$32,$00
        ;.db $E8,$3F,$DA,$1F,$2E,$20,$08,$40,$00,$C0
        
pupl50  vctr(-4d,8d,hidden)       ;BA8
        jsrl(pupl51_)   ;$6BB6
        jsrl(pupl51)    ;$6BB2
        vctr(-22d,-18d,hidden)
        rtsl
        ;.db $1E,$44,$DB,$B5,$D9,$B5,$15,$57,$00,$C0
        
pupl51  vctr(46d,-14d,hidden)     ;BB2
pupl51_ vctr(-6d,-8d,visible)
        vctr(-10d,12d,visible)
        vctr(6d,8d,visible)
        rtsl
        ;.db $F2,$1F,$2E,$00,$3D,$5C,$3B,$46,$23,$44,$00,$C0
        
pupl52  vctr(18d,24d,visible)     ;BBE
        vctr(16d,-22d,visible)
        vctr(-4d,-6d,visible)
        vctr(-30d,6d,hidden)
        vctr(-18d,24d,visible)
        vctr(-16d,-22d,visible)
        vctr(6d,-8d,visible)
        vctr(-16d,-22d,visible)
        vctr(-2d,-84d,visible)
        vctr(78d,48d,visible)
        rtsl
        ;.db $29,$4C,$28,$55,$3E,$5D,$11,$43,$37,$4C,$38,$55,$23,$5C,$38,$55
        ;.db $AC,$1F,$FE,$3F,$30,$00,$4E,$20,$00,$C0
        
pupl53  vctr(-6d,14d,visible)     ;BD8
        vctr(-36d,4d,visible)
        vctr(-24d,-4d,visible)
        rtsl
        ;.db $3D,$47,$04,$00,$DC,$3F,$34,$5E,$00,$C0
        
wing53  jsrl(pupl52)    ;$6BBE
        vctr(4d,14d,visible)
        vctr(-12d,8d,hidden)
        vctr(68d,-36d,visible)
        vctr(-54d,58d,visible)
        jsrl(pupl53)    ;$6BD8
        vctr(-16d,-32d,hidden)
        vctr(-56d,-22d,visible)
        vctr(68d,48d,visible)
        vctr(32d,10d,hidden)
        rtsl
        ;.db $DF,$B5,$22,$47,$1A,$44,$DC,$1F,$44,$20,$3A,$00,$CA,$3F,$EC,$B5
        ;.db $E0,$1F,$F0,$1F,$EA,$1F,$C8,$3F,$30,$00,$44,$20,$0A,$00,$20,$00
        ;.db $00,$C0
        
wing52  jsrl(pupl52)    ;$6BBE
        vctr(6d,20d,visible)
        vctr(-14d,2d,hidden)
        vctr(72d,-10d,visible)
        vctr(-58d,32d,visible)
        jsrl(pupl53)    ;$6BD8
        vctr(-16d,-30d,hidden)
        vctr(-56d,0d,visible)
        vctr(70d,26d,visible)
        vctr(30d,8d,hidden)
        rtsl
        ;.db $DF,$B5,$23,$4A,$19,$41,$F6,$1F,$48,$20,$20,$00,$C6,$3F,$EC,$B5
        ;.db $18,$51,$00,$00,$C8,$3F,$1A,$00,$46,$20,$0F,$44,$00,$C0
        
wing51  jsrl(pupl52)    ;$6BBE
        vctr(8d,24d,visible)
        vctr(-16d,-2d,hidden)
        vctr(80d,16d,visible)
        vctr(-66d,6d,visible)
        jsrl(pupl53)    ;$6BD8
        vctr(-16d,-26d,hidden)
        vctr(-56d,22d,visible)
        vctr(70d,0d,visible)
        vctr(30d,8d,hidden)
        rtsl
        ;.db $DF,$B5,$24,$4C,$18,$5F,$10,$00,$50,$20,$06,$00,$BE,$3F,$EC,$B5
        ;.db $18,$53,$16,$00,$C8,$3F,$00,$00,$46,$20,$0F,$44,$00,$C0
        
wing50  jsrl(pupl52)    ;$6BBE
        vctr(10d,30d,visible)
        vctr(-18d,-8d,hidden)
        vctr(82d,42d,visible)
        vctr(-68d,-20d,visible)
        jsrl(pupl53)    ;$6BD8
        vctr(-16d,-22d,hidden)
        vctr(-56d,42d,visible)
        vctr(70d,-22d,visible)
        vctr(30d,6d,hidden)
        rtsl
        ;.db $DF,$B5,$25,$4F,$17,$5C,$2A,$00,$52,$20,$EC,$1F,$BC,$3F,$EC,$B5
        ;.db $18,$55,$2A,$00,$C8,$3F,$EA,$1F,$46,$20,$0F,$43,$00,$C0
        
bang0   vctr(-184d,28d,hidden)
        vctr(148d,36d,visible)
        vctr(64d,96d,visible)
        vctr(40d,-112d,visible)
        vctr(144d,-40d,visible)
        vctr(-176d,-52d,visible)
        vctr(-40d,-112d,visible)
        vctr(-56d,140d,visible)
        vctr(-124d,44d,visible)
        vctr(184d,-28d,hidden)
        rtsl
        ;.db $1C,$00,$48,$1F,$24,$00,$94,$20,$60,$00,$40,$20,$90,$1F,$28,$20
        ;.db $D8,$1F,$90,$20,$CC,$1F,$50,$3F,$90,$1F,$D8,$3F,$8C,$00,$C8,$3F
        ;.db $2C,$00,$84,$3F,$E4,$1F,$B8,$00,$00,$C0
        
ngwi0   vctr(-32d,-56d,hidden)
        vctr(56d,48d,visible)
        vctr(8d,16d,visible)
        vctr(-12d,0d,visible)
        vctr(4d,16d,visible)
        vctr(-24d,0d,visible)
        vctr(0d,16d,visible)
        vctr(-12d,0d,visible)
        vctr(-20d,-96d,visible)
        vctr(32d,56d,hidden)
        rtsl
        ;.db $C8,$1F,$E0,$1F,$30,$00,$38,$20,$24,$48,$3A,$40,$22,$48,$34,$40
        ;.db $20,$48,$3A,$40,$A0,$1F,$EC,$3F,$38,$00,$20,$00,$00,$C0
        
ngwi1   vctr(-64d,0d,hidden)
        vctr(88d,-28d,visible)
        vctr(16d,8d,visible)
        vctr(-6d,12d,visible)
        vctr(14d,6d,visible)
        vctr(-16d,18d,visible)
        vctr(16d,8d,visible)
        vctr(-12d,8d,visible)
        vctr(-100d,-32d,visible)
        vctr(64d,0d,hidden)
        rtsl
        ;.db $00,$00,$C0,$1F,$E4,$1F,$58,$20,$28,$44,$3D,$46,$27,$43,$38,$49
        ;.db $28,$44,$3A,$44,$E0,$1F,$9C,$3F,$00,$00,$40,$00,$00,$C0
        
ngwi2   vctr(-32d,54d,hidden)
        vctr(76d,-66d,visible)
        vctr(-4d,-16d,visible)
        vctr(-16d,12d,visible)
        vctr(-12d,-20d,visible)
        vctr(-12d,12d,visible)
        vctr(-8d,-8d,visible)
        vctr(-12d,12d,visible)
        vctr(-12d,74d,visible)
        vctr(32d,-54d,hidden)
        rtsl
        ;.db $36,$00,$E0,$1F,$BE,$1F,$4C,$20,$3E,$58,$38,$46,$3A,$56,$3A,$46
        ;.db $3C,$5C,$3A,$46,$4A,$00,$F4,$3F,$CA,$1F,$20,$00,$00,$C0
        
ngwi3   vctr(32d,54d,hidden)
        vctr(-76d,-66d,visible)
        vctr(4d,-16d,visible)
        vctr(16d,12d,visible)
        vctr(12d,-20d,visible)
        vctr(12d,12d,visible)
        vctr(8d,-8d,visible)
        vctr(12d,12d,visible)
        vctr(12d,74d,visible)
        vctr(-32d,-54d,hidden)
        rtsl
        ;.db $36,$00,$20,$00,$BE,$1F,$B4,$3F,$22,$58,$28,$46,$26,$56,$26,$46
        ;.db $24,$5C,$26,$46,$4A,$00,$0C,$20,$CA,$1F,$E0,$1F,$00,$C0
        
ngwi4   vctr(64d,0d,hidden)
        vctr(-88d,-28d,visible)
        vctr(-16d,8d,visible)
        vctr(6d,12d,visible)
        vctr(-14d,6d,visible)
        vctr(16d,18d,visible)
        vctr(-16d,8d,visible)
        vctr(12d,8d,visible)
        vctr(100d,-32d,visible)
        vctr(-64d,0d,hidden)
        rtsl
        ;.db $00,$00,$40,$00,$E4,$1F,$A8,$3F,$38,$44,$23,$46,$39,$43,$28,$49
        ;.db $38,$44,$26,$44,$E0,$1F,$64,$20,$00,$00,$C0,$1F,$00,$C0
        
ngwi5   vctr(32d,-56d,hidden)
        vctr(-56d,48d,visible)
        vctr(-8d,16d,visible)
        vctr(12d,0d,visible)
        vctr(-4d,16d,visible)
        vctr(24d,0d,visible)
        vctr(0d,16d,visible)
        vctr(12d,0d,visible)
        vctr(20d,-96d,visible)
        vctr(-32d,56d,hidden)
        rtsl
        ;.db $C8,$1F,$20,$00,$30,$00,$C8,$3F,$3C,$48,$26,$40,$3E,$48,$2C,$40
        ;.db $20,$48,$26,$40,$A0,$1F,$14,$20,$38,$00,$E0,$1F,$00,$C0
        
coil0   vctr(4d,4d,hidden)
        vctr(4d,-4d,visible)
        vctr(-6d,-8d,visible)
        vctr(-10d,8d,visible)
        vctr(12d,16d,visible)
        vctr(20d,-16d,visible)
        vctr(-20d,-24d,visible)
        vctr(-26d,22d,visible)
        vctr(22d,2d,hidden)
        rtsl    
        ;.db $02,$42,$22,$5E,$3D,$5C,$3B,$44,$26,$48,$2A,$58,$36,$54,$33,$4B
        ;.db $0B,$41,$00,$C0
        
coil1   vctr(-2d,6d,hidden)
        vctr(4d,2d,visible)
        vctr(6d,-10d,visible)
        vctr(-10d,-6d,visible)
        vctr(-10d,18d,visible)
        vctr(22d,12d,visible)
        vctr(14d,-26d,visible)
        vctr(-32d,-18d,visible)
        vctr(8d,22d,hidden)
        rtsl
        ;.db $1F,$43,$22,$41,$23,$5B,$3B,$5D,$3B,$49,$2B,$46,$27,$53,$EE,$1F
        ;.db $E0,$3F,$04,$4B,$00,$C0
        
coil2   vctr(-6d,2d,hidden)
        vctr(0d,4d,visible)
        vctr(12d,0d,visible)
        vctr(0d,-12d,visible)
        vctr(-22d,0d,visible)
        vctr(0d,26d,visible)
        vctr(30d,0d,visible)
        vctr(0d,-36d,visible)
        vctr(-14d,16d,hidden)
        rtsl
        ;.db $1D,$41,$20,$42,$26,$40,$20,$5A,$35,$40,$20,$4D,$2F,$40,$DC,$1F
        ;.db $00,$20,$19,$48,$00,$C0
        
coil3   vctr(-4d,-4d,hidden)
        vctr(-4d,4d,visible)
        vctr(6d,8d,visible)
        vctr(10d,-8d,visible)
        vctr(-12d,-16d,visible)
        vctr(-20d,16d,visible)
        vctr(20d,24d,visible)
        vctr(26d,-22d,visible)
        vctr(-22d,-2d,hidden)
        rtsl
        ;.db $1E,$5E,$3E,$42,$23,$44,$25,$5C,$3A,$58,$36,$48,$2A,$4C,$2D,$55
        ;.db $15,$5F,$00,$C0
        
coil4   vctr(2d,-6d,hidden)
        vctr(-4d,-2d,visible)
        vctr(-6d,10d,visible)
        vctr(10d,6d,visible)
        vctr(10d,-18d,visible)
        vctr(-22d,-12d,visible)
        vctr(-14d,26d,visible)
        vctr(32d,18d,visible)
        vctr(-8d,-22d,hidden)
        rtsl
        ;.db $01,$5D,$3E,$5F,$3D,$45,$25,$43,$25,$57,$35,$5A,$39,$4D,$12,$00
        ;.db $20,$20,$1C,$55,$00,$C0
        
coil5   vctr(6d,-2d,hidden)
        vctr(0d,-4d,visible)
        vctr(-12d,0d,visible)
        vctr(0d,12d,visible)
        vctr(22d,0d,visible)
        vctr(0d,-26d,visible)
        vctr(-30d,0d,visible)
        vctr(0d,36d,visible)
        vctr(14d,-16d,hidden)
        rtsl
        ;.db $03,$5F,$20,$5E,$3A,$40,$20,$46,$2B,$40,$20,$53,$31,$40,$24,$00
        ;.db $00,$20,$07,$58,$00,$C0
        
flap0   vctr(-30d,14d,visible)
        vctr(0d,-8d,visible)
        vctr(14d,-14d,visible)
        vctr(16d,0d,visible)
        vctr(0d,16d,visible)
        vctr(-14d,14d,visible)
        vctr(-16d,0d,visible)
        vctr(0d,-8d,visible)
        rtsl
        ;.db $31,$47,$20,$5C,$27,$59,$28,$40,$20,$48,$39,$47,$38,$40,$20,$5C
        ;.db $00,$C0
        
flap1   vctr(-12d,32d,visible)
        vctr(-6d,-6d,visible)
        vctr(0d,-20d,visible)
        vctr(12d,-12d,visible)
        vctr(12d,12d,visible)
        vctr(0d,20d,visible)
        vctr(-12d,6d,visible)
        vctr(-6d,0d,visible)
        rtsl
        ;.db $20,$00,$F4,$3F,$3D,$5D,$20,$56,$26,$5A,$26,$46,$20,$4A,$3A,$43
        ;.db $3D,$40,$00,$C0
        
flap2   vctr(14d,30d,visible)
        vctr(-8d,0d,visible)
        vctr(-14d,-14d,visible)
        vctr(0d,-16d,visible)
        vctr(16d,0d,visible)
        vctr(14d,14d,visible)
        vctr(0d,16d,visible)
        vctr(-8d,0d,visible)
        rtsl
        ;.db $27,$4F,$3C,$40,$39,$59,$20,$58,$28,$40,$27,$47,$20,$48,$3C,$40
        ;.db $00,$C0
        
wdgt0   vctr(12d,20d,hidden)
        vctr(0d,-12d,visible)
        vctr(-16d,-28d,visible)
        vctr(-8d,0d,visible)
        vctr(0d,12d,visible)
        vctr(16d,28d,visible)
        vctr(14d,0d,visible)
        vctr(0d,-12d,visible)
        vctr(-16d,-28d,visible)
        vctr(-6d,0d,visible)
        vctr(-2d,6d,hidden)
        vctr(0d,8d,visible)
        vctr(12d,12d,hidden)
        vctr(0d,8d,visible)
        vctr(-6d,-14d,hidden)
        rtsl
        ;.db $06,$4A,$20,$5A,$38,$52,$3C,$40,$20,$46,$28,$4E,$27,$40,$20,$5A
        ;.db $38,$52,$3D,$40,$1F,$43,$20,$44,$06,$46,$20,$44,$1D,$59,$00,$C0

wdgt1   vctr(26d,0d,hidden)
        vctr(-6d,-6d,visible)
        vctr(-40d,0d,visible)
        vctr(-6d,6d,visible)
        vctr(6d,6d,visible)
        vctr(40d,0d,visible)
        vctr(6d,-6d,visible)
        vctr(0d,-6d,visible)
        vctr(-6d,-6d,visible)
        vctr(-40d,0d,visible)
        vctr(-6d,6d,visible)
        vctr(0d,6d,visible)
        vctr(6d,0d,hidden)
        vctr(8d,0d,visible)
        vctr(24d,0d,hidden)
        vctr(8d,0d,visible)
        vctr(-20d,0d,hidden)
        rtsl
        ;.db $0D,$40,$3D,$5D,$00,$00,$D8,$3F,$3D,$43,$23,$43,$00,$00,$28,$20
        ;.db $23,$5D,$20,$5D,$3D,$5D,$00,$00,$D8,$3F,$3D,$43,$20,$43,$03,$40
        ;.db $24,$40,$0C,$40,$24,$40,$16,$40,$00,$C0
        
wdgt2   vctr(-20d,4d,hidden)
        vctr(28d,-16d,visible)
        vctr(12d,0d,visible)
        vctr(0d,8d,visible)
        vctr(-28d,16d,visible)
        vctr(-12d,0d,visible)
        vctr(0d,-14d,visible)
        vctr(28d,-16d,visible)
        vctr(12d,0d,visible)
        vctr(0d,6d,visible)
        vctr(-12d,6d,hidden)
        vctr(8d,0d,visible)
        vctr(-32d,12d,hidden)
        vctr(8d,0d,visible)
        vctr(8d,-6d,hidden)
        rtsl
        ;.db $16,$42,$2E,$58,$26,$40,$20,$44,$32,$48,$3A,$40,$20,$59,$2E,$58
        ;.db $26,$40,$20,$43,$1A,$43,$24,$40,$0C,$00,$E0,$1F,$24,$40,$04,$5D
        ;.db $00,$C0
        
wdgt3   vctr(8d,-16d,hidden)
        vctr(-8d,-4d,visible)
        vctr(-8d,4d,visible)
        vctr(0d,32d,visible)
        vctr(8d,4d,visible)
        vctr(8d,-4d,visible)
        vctr(0d,-38d,visible)
        vctr(-8d,-4d,visible)
        vctr(-8d,4d,visible)
        vctr(0d,6d,visible)
        vctr(8d,0d,hidden)
        vctr(0d,8d,visible)
        vctr(0d,16d,hidden)
        vctr(0d,8d,visible)
        vctr(0d,-16d,hidden)
        rtsl
        ;.db $04,$58,$3C,$5E,$3C,$42,$20,$00,$00,$20,$24,$42,$24,$5E,$DA,$1F
        ;.db $00,$20,$3C,$5E,$3C,$42,$20,$43,$04,$40,$20,$44,$00,$48,$20,$44
        ;.db $00,$58,$00,$C0
    
;**************************************
    .sbttl "Clock Parts"
;**************************************
clock7 = $60|page

sqr     vctr(40d,40d,hidden)
        vctr(0d,-80d,visible)
        vctr(-80d,0d,visible)
        vctr(0d,80d,visible)
        vctr(80d,0d,visible)
        vctr(-16d,-16d,visible)
        vctr(6d,-12d,visible)
        vctr(2d,-12d,visible)
        vctr(-2d,-12d,visible)
        vctr(-6d,-12d,visible)
        vctr(-12d,-6d,visible)
        vctr(-12d,-2d,visible)
        vctr(-12d,2d,visible)
        vctr(-12d,6d,visible)
        vctr(-6d,12d,visible)
        vctr(-2d,12d,visible)
        vctr(2d,12d,visible)
        vctr(6d,12d,visible)
        vctr(12d,6d,visible)
        vctr(12d,2d,visible)
        vctr(12d,-2d,visible)
        vctr(12d,-6d,visible)
        vctr(0d,-48d,hidden)
        vctr(16d,-16d,visible)
        vctr(-80d,0d,hidden)
        vctr(16d,16d,visible)
        vctr(0d,48d,hidden)
        vctr(-16d,16d,visible)
        vctr(40d,-40d,hidden)
        rtsl
        ;.db $28,$00,$28,$00,$B0,$1F,$00,$20,$00,$00,$B0,$3F,$50,$00,$00,$20
        ;.db $00,$00,$50,$20,$38,$58,$23,$5A,$21,$5A,$3F,$5A,$3D,$5A,$3A,$5D
        ;.db $3A,$5F,$3A,$41,$3A,$43,$3D,$46,$3F,$46,$21,$46,$23,$46,$26,$43
        ;.db $26,$41,$26,$5F,$26,$5D,$D0,$1F,$00,$00,$28,$58,$00,$00,$B0,$1F
        ;.db $28,$48,$30,$00,$00,$00,$38,$48,$D8,$1F,$28,$00,$00,$C0
        
dial    vctr(0d,24d,hidden)
        vctr(2d,4d,visible)
        vctr(-4d,0d,visible)
        vctr(2d,-4d,visible)
        vctr(24d,-24d,hidden)
        vctr(4d,2d,visible)
        vctr(0d,-4d,visible)
        vctr(-4d,2d,visible)
        vctr(-24d,-24d,hidden)
        vctr(2d,-4d,visible)
        vctr(-4d,0d,visible)
        vctr(2d,4d,visible)
        vctr(-24d,24d,hidden)
        vctr(-4d,-2d,visible)
        vctr(0d,4d,visible)
        vctr(4d,-2d,visible)
        vctr(24d,0d,hidden)
        rtsl
        ;.db $00,$4C,$21,$42,$3E,$40,$21,$5E,$0C,$54,$22,$41,$20,$5E,$3E,$41
        ;.db $14,$54,$21,$5E,$3E,$40,$21,$42,$14,$4C,$3E,$5F,$20,$42,$22,$5F
        ;.db $0C,$40,$00,$C0
        
;**************************************
    .sbttl "Boots"
;**************************************
boot7 = $60|page

shoes   vctr(-12d,0d,hidden)
        vctr(0d,12d,visible)
        vctr(2d,2d,visible)
        vctr(8d,0d,visible)
        vctr(-2d,-2d,visible)
        vctr(-8d,0d,visible)
        vctr(10d,2d,hidden)
        vctr(0d,-8d,visible)
        vctr(-1d,-6d,visible)
        vctr(1d,-4d,visible)
        vctr(-2d,-6d,visible)
        vctr(-2d,0d,visible)
        vctr(0d,2d,visible)
        vctr(-2d,-4d,visible)
        vctr(-4d,-4d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,4d,visible)
        vctr(4d,4d,visible)
        vctr(2d,8d,visible)
        vctr(4d,0d,visible)
        vctr(0d,-6d,visible)
        vctr(14d,12d,hidden)
        vctr(0d,6d,visible)
        vctr(-4d,2d,visible)
        vctr(6d,0d,visible)
        vctr(4d,-2d,visible)
        vctr(-6d,0d,visible)
        vctr(6d,0d,hidden)
        vctr(0d,-10d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,-6d,visible)
        vctr(4d,6d,hidden)
        vctr(6d,-6d,visible)
        vctr(6d,-2d,visible)
        vctr(0d,-2d,visible)
        vctr(-4d,-2d,visible)
        vctr(-4d,0d,visible)
        vctr(-6d,2d,visible)
        vctr(0d,-2d,visible)
        vctr(-6d,0d,visible)
        vctr(-2d,4d,visible)
        vctr(3d,5d,visible)
        vctr(-3d,9d,visible)
        vctr(0d,6d,visible)
        rtsl
        ;.db $1A,$40,$20,$46,$21,$41,$24,$40,$3F,$5F,$3C,$40,$05,$41,$20,$5C
        ;.db $FA,$1F,$FF,$3F,$FC,$1F,$01,$20,$3F,$5D,$3F,$40,$20,$41,$3F,$5E
        ;.db $3E,$5E,$3D,$40,$20,$42,$22,$42,$21,$44,$22,$40,$20,$5D,$07,$46
        ;.db $20,$43,$3E,$41,$23,$40,$22,$5F,$3D,$40,$03,$40,$20,$5B,$3E,$40
        ;.db $20,$5D,$02,$43,$23,$5D,$23,$5F,$20,$5F,$3E,$5F,$3E,$40,$3D,$41
        ;.db $20,$5F,$3D,$40,$3F,$42,$05,$00,$03,$20,$09,$00,$FD,$3F,$20,$43
        ;.db $00,$C0
        
bootz1  vctr(-36d,-70d,hidden)
        vctr(0d,-18d,visible)
        vctr(30d,0d,visible)
        vctr(-6d,18d,visible)
        vctr(26d,0d,hidden)
        vctr(-6d,-18d,visible)
        vctr(30d,0d,visible)
        vctr(0d,18d,visible)
        vctr(-38d,70d,hidden)
        rtsl
        ;.db $BA,$1F,$DC,$1F,$20,$57,$2F,$40,$3D,$49,$0D,$40,$3D,$57,$2F,$40
        ;.db $20,$49,$46,$00,$DA,$1F,$00,$C0
        
bootz2  vctr(-36d,-64d,hidden)
        vctr(2d,-18d,visible)
        vctr(-12d,-4d,visible)
        vctr(0d,-6d,visible)
        vctr(14d,0d,visible)
        vctr(12d,6d,visible)
        vctr(0d,-4d,visible)
        vctr(12d,2d,visible)
        vctr(0d,8d,visible)
        vctr(-4d,4d,visible)
        vctr(0d,12d,visible)
        vctr(-24d,0d,visible)
        vctr(50d,0d,hidden)
        vctr(0d,-12d,visible)
        vctr(-4d,-4d,visible)
        vctr(0d,-8d,visible)
        vctr(10d,-2d,visible)
        vctr(0d,2d,visible)
        vctr(12d,-4d,visible)
        vctr(14d,0d,visible)
        vctr(0d,6d,visible)
        vctr(-10d,4d,visible)
        vctr(2d,18d,visible)
        vctr(-24d,0d,visible)
        vctr(-14d,64d,hidden)
        rtsl
        ;.db $C0,$1F,$DC,$1F,$21,$57,$3A,$5E,$20,$5D,$27,$40,$26,$43,$20,$5E
        ;.db $26,$41,$20,$44,$3E,$42,$20,$46,$34,$40,$00,$00,$32,$00,$20,$5A
        ;.db $3E,$5E,$20,$5C,$25,$5F,$20,$41,$26,$5E,$27,$40,$20,$43,$3B,$42
        ;.db $21,$49,$34,$40,$40,$00,$F2,$1F,$00,$C0
        
magic0  vctr(-30d,-96d,hidden)
        vctr(10d,-8d,visible)
        vctr(40d,0d,visible)
        vctr(10d,8d,visible)
        vctr(-30d,96d,hidden)
        rtsl
        ;.db $A0,$1F,$E2,$1F,$25,$5C,$00,$00,$28,$20,$25,$44,$60,$00,$E2,$1F
        ;.db $00,$C0
        
magic1  vctr(-34d,-98d,hidden)
        vctr(14d,-10d,visible)
        vctr(40d,0d,visible)
        vctr(14d,10d,visible)
        vctr(-34d,98d,hidden)
        rtsl    
        ;.db $9E,$1F,$DE,$1F,$27,$5B,$00,$00,$28,$20,$27,$45,$62,$00,$DE,$1F
        ;.db $00,$C0
        
magic2  vctr(-38d,-102d,hidden)
        vctr(18d,-12d,visible)
        vctr(40d,0d,visible)
        vctr(18d,12d,visible)
        vctr(-38d,102d,hidden)
        rtsl
        ;.db $9A,$1F,$DA,$1F,$29,$5A,$00,$00,$28,$20,$29,$46,$66,$00,$DA,$1F
        ;.db $00,$C0
        
magic3  vctr(-46d,-106d,hidden)
        vctr(26d,-18d,visible)
        vctr(40d,0d,visible)
        vctr(26d,18d,visible)
        vctr(-46d,106d,hidden)
        rtsl
        ;.db $96,$1F,$D2,$1F,$2D,$57,$00,$00,$28,$20,$2D,$49,$6A,$00,$D2,$1F
        ;.db $00,$C0
        
;************************************
    .sbttl "Stalactite"
;**************************************
tite7 = $60|page

tite_   vctr(4d,14d,visible)      ;FF2
        vctr(4d,-6d,visible)
        vctr(6d,16d,visible)
        vctr(6d,-24d,visible)
        rtsl        
        ;.db $22,$47,$22,$5D,$23,$48,$23,$54,$00,$C0
        
ovhng   vctr(26d,16d,hidden)
        vctr(-52d,0d,visible)
        vctr(6d,-32d,visible)
        jsrl(tite_)     ;$6FF2
        jsrl(tite_)     ;$6FF2
        vctr(6d,32d,visible)
        rtsl
        ;.db $0D,$48,$00,$00,$CC,$3F,$E0,$1F,$06,$20,$F9,$B7,$F9,$B7,$20,$00
        ;.db $06,$20,$00,$C0
        
;************************************
    .sbttl "Locks and Keys"
;**************************************
lock7 = $60|page

key     vctr(0d,6d,hidden)
        vctr(4d,2d,visible)
        vctr(0d,5d,visible)
        vctr(-4d,3d,visible)
        vctr(-4d,-3d,visible)
        vctr(0d,-5d,visible)
        vctr(4d,-2d,visible)
        vctr(0d,-18d,visible)
        vctr(-6d,0d,visible)
        vctr(2d,2d,hidden)
        vctr(4d,0d,visible)
        vctr(0d,2d,hidden)
        vctr(-4d,0d,visible)
        rtsl
        ;.db $00,$43,$22,$41,$05,$00,$00,$20,$03,$00,$FC,$3F,$FD,$1F,$FC,$3F
        ;.db $FB,$1F,$00,$20,$22,$5F,$20,$57,$3D,$40,$01,$41,$22,$40,$00,$41
        ;.db $3E,$40,$00,$C0
        
lock    vctr(1d,0d,hidden)
        vctr(3d,2d,visible)
        vctr(0d,4d,visible)
        vctr(-4d,3d,visible)
        vctr(-4d,-3d,visible)
        vctr(0d,-4d,visible)
        vctr(3d,-2d,visible)
        vctr(-2d,-10d,visible)
        vctr(6d,0d,visible)
        vctr(-2d,10d,visible)
        vctr(-9d,12d,hidden)
        vctr(16d,0d,visible)
        vctr(0d,-26d,visible)
        vctr(-16d,0d,visible)
        vctr(0d,26d,visible)
        vctr(8d,0d,visible)
        vctr(0d,40d,visible)
        vctr(0d,-66d,hidden)
        vctr(0d,-38d,visible)
        rtsl
        ;.db $00,$00,$01,$00,$02,$00,$03,$20,$20,$42,$03,$00,$FC,$3F,$FD,$1F
        ;.db $FC,$3F,$20,$5E,$FE,$1F,$03,$20,$3F,$5B,$23,$40,$3F,$45,$0C,$00
        ;.db $F7,$1F,$28,$40,$20,$53,$38,$40,$20,$4D,$24,$40,$28,$00,$00,$20
        ;.db $BE,$1F,$00,$00,$DA,$1F,$00,$20,$00,$C0

        ;************************************
    .sbttl "Transporter"
;**************************************
tran7 = $60|page
        
booth   vctr(-18d,24d,hidden)
        vctr(2d,6d,visible)
        vctr(6d,2d,visible)
        vctr(20d,0d,visible)
        vctr(6d,-2d,visible)
        vctr(2d,-6d,visible)
        vctr(0d,-48d,visible)
        vctr(-2d,-6d,visible)
        vctr(-6d,-2d,visible)
        vctr(-20d,0d,visible)
        vctr(-6d,2d,visible)
        vctr(-2d,6d,visible)
        vctr(0d,48d,visible)
        vctr(-12d,12d,hidden)
        vctr(2d,6d,visible)
        vctr(6d,2d,visible)
        vctr(44d,0d,visible)
        vctr(6d,-2d,visible)
        vctr(2d,-6d,visible)
        vctr(0d,-72d,visible)
        vctr(-2d,-6d,visible)
        vctr(-6d,-2d,visible)
        vctr(-44d,0d,visible)
        vctr(-6d,2d,visible)
        vctr(-2d,6d,visible)
        vctr(0d,72d,visible)
        vctr(30d,-36d,hidden)
        rtsl
        ;.db $17,$4C,$21,$43,$23,$41,$2A,$40,$23,$5F,$21,$5D,$D0,$1F,$00,$20
        ;.db $3F,$5D,$3D,$5F,$36,$40,$3D,$41,$3F,$43,$30,$00,$00,$20,$1A,$46
        ;.db $21,$43,$23,$41,$00,$00,$2C,$20,$23,$5F,$21,$5D,$B8,$1F,$00,$20
        ;.db $3F,$5D,$3D,$5F,$00,$00,$D4,$3F,$3D,$41,$3F,$43,$48,$00,$00,$20
        ;.db $DC,$1F,$1E,$00,$00,$C0
        
nutz    vctr(-23d,32d,hidden)
        jsrl(nut0)  ;$70D2
        vctr(46d,0d,hidden)
        jsrl(nut0)  ;$70D2
        vctr(0d,-64d,hidden)
        jsrl(nut0)  ;$70D2
        vctr(-46d,0d,hidden)
        jsrl(nut0)  ;$70D2
        vctr(23d,32d,hidden)
        rtsl
        ;.db $20,$00,$E9,$1F,$69,$B8,$00,$00,$2E,$00,$69,$B8,$C0,$1F,$00,$00
        ;.db $69,$B8,$00,$00,$D2,$1F,$69,$B8,$20,$00,$17,$00,$00,$C0
        
nut0    vctr(-3d,2d,visible)
        vctr(0d,3d,visible)
        vctr(3d,2d,visible)
        vctr(3d,-2d,visible)
        vctr(0d,-3d,visible)
        vctr(-3d,-2d,visible)
        rtsl
        ;.db $02,$00,$FD,$3F,$03,$00,$00,$20,$02,$00,$03,$20,$FE,$1F,$03,$20
        ;.db $FD,$1F,$00,$20,$FE,$1F,$FD,$3F,$00,$C0
        
star0   vctr(2d,0d,hidden)
        vctr(-4d,0d,visible)
        vctr(2d,-2d,hidden)
        vctr(0d,4d,visible)
        vctr(0d,-2d,hidden)
        rtsl
        ;.db $01,$40,$3E,$40,$01,$5F,$20,$42,$00,$5F,$00,$C0

star1   vctr(4d,0d,hidden)
        vctr(-8d,0d,visible)
        vctr(4d,-4d,hidden)
        vctr(0d,8d,visible)
        vctr(0d,-4d,hidden)
        rtsl
        ;.db $02,$40,$3C,$40,$02,$5E,$20,$44,$00,$5E,$00,$C0

star2   vctr(6d,0d,hidden)
        vctr(-12d,0d,visible)
        vctr(6d,-6d,hidden)
        vctr(0d,12d,visible)
        vctr(0d,-6d,hidden)
        rtsl
        ;.db $03,$40,$3A,$40,$03,$5D,$20,$46,$00,$5D,$00,$C0

star3   vctr(8d,0d,hidden)
        vctr(-16d,0d,visible)
        vctr(8d,-8d,hidden)
        vctr(0d,16d,visible)
        vctr(0d,-8d,hidden)
        rtsl
        ;.db $04,$40,$38,$40,$04,$5C,$20,$48,$00,$5C,$00,$C0

;************************************
;* Hand Parts
;**************************************
hand7 = $60|page 

hand    vctr(6d,0d,visible)
        vctr(4d,2d,visible)
        vctr(22d,0d,visible)
        vctr(0d,-2d,visible)
        vctr(-4d,-2d,visible)
        vctr(-10d,0d,visible)
        vctr(0d,4d,hidden)
        vctr(0d,-6d,visible)
        vctr(-4d,0d,visible)
        vctr(-2d,0d,hidden)
        vctr(2d,4d,visible)
        vctr(0d,-6d,visible)
        vctr(8d,0d,visible)
        vctr(0d,4d,visible)
        vctr(-2d,-4d,hidden)
        vctr(0d,-4d,visible)
        vctr(-8d,0d,visible)
        vctr(0d,4d,visible)
        vctr(2d,0d,visible)
        vctr(4d,-4d,hidden)
        vctr(0d,-4d,visible)
        vctr(-6d,0d,visible)
        vctr(0d,4d,visible)
        vctr(0d,-4d,hidden)
        vctr(-4d,0d,visible)
        vctr(-4d,2d,visible)
        vctr(-4d,0d,visible)
        vctr(0d,12d,visible)
        rtsl
        ;.db $23,$40,$22,$41,$2B,$40,$20,$5F,$3E,$5F,$3B,$40,$00,$42,$20,$5D
        ;.db $3E,$40,$1F,$40,$21,$42,$20,$5D,$24,$40,$20,$42,$1F,$5E,$20,$5E
        ;.db $3C,$40,$20,$42,$21,$40,$02,$5E,$20,$5E,$3D,$40,$20,$42,$00,$5E
        ;.db $3E,$40,$3E,$41,$3E,$40,$20,$46,$00,$C0
        
box     vctr(16d,0d,visible)
        vctr(0d,-16d,visible)
        vctr(-32d,0d,visible)
        vctr(0d,16d,visible)
        vctr(16d,0d,visible)
        vctr(-2d,0d,hidden)
        rtsl
        ;.db $28,$40,$20,$58,$00,$00,$E0,$3F,$20,$48,$28,$40,$1F,$40,$00,$C0

swtch0  vctr(8d,16d,visible)
        vctr(4d,0d,visible)
        vctr(-8d,-16d,visible)
        vctr(-2d,-16d,hidden)
        rtsl
        ;.db $24,$48,$22,$40,$3C,$58,$1F,$58,$00,$C0

swtch1  vctr(-8d,16d,visible)
        vctr(4d,0d,visible)
        vctr(8d,-16d,visible)
        vctr(-2d,-16d,hidden)
        rtsl
        ;.db $3C,$48,$22,$40,$24,$58,$1F,$58,$00,$C0

;************************************
    .sbttl "Escape Pod"
;**************************************
pod7 = $60|page

escpod  vctr(-24d,-144d,hidden)
        vctr(-72d,24d,visible)
        vctr(0d,216d,visible)
        vctr(96d,96d,visible)
        vctr(96d,-96d,visible)
        vctr(0d,-216d,visible)
        vctr(-72d,-24d,visible)
        vctr(24d,-32d,visible)
        vctr(-96d,0d,visible)
        vctr(24d,32d,visible)
        vctr(24d,144d,hidden)
        rtsl
        ;.db $70,$1F,$E8,$1F,$18,$00,$B8,$3F,$D8,$00,$00,$20,$60,$00,$60,$20
        ;.db $A0,$1F,$60,$20,$28,$1F,$00,$20,$E8,$1F,$B8,$3F,$E0,$1F,$18,$20
        ;.db $00,$00,$A0,$3F,$20,$00,$18,$20,$90,$00,$18,$00,$00,$C0
        
flame0  vctr(-8d,-192d,hidden)
        vctr(8d,-24d,visible)
        vctr(8d,24d,visible)
        vctr(-8d,192d,hidden)
        rtsl
        ;.db $40,$1F,$F8,$1F,$24,$54,$24,$4C,$C0,$00,$F8,$1F,$00,$C0

flame1  vctr(-16d,-192d,hidden)
        vctr(16d,-48d,visible)
        vctr(16d,48d,visible)
        vctr(-16d,192d,hidden)
        rtsl
        ;.db $40,$1F,$F0,$1F,$D0,$1F,$10,$20,$30,$00,$10,$20,$C0,$00,$F0,$1F
        ;.db $00,$C0
        
flame2  vctr(-24d,-192d,hidden)
        vctr(24d,-72d,visible)
        vctr(24d,72d,visible)
        vctr(-24d,192d,hidden)
        rtsl
        ;.db $40,$1F,$E8,$1F,$B8,$1F,$18,$20,$48,$00,$18,$20,$C0,$00,$E8,$1F
        ;.db $00,$C0
        
flame3  vctr(-32d,-192d,hidden)
        vctr(32d,-96d,visible)
        vctr(32d,96d,visible)
        vctr(-32d,192d,hidden)
        rtsl
        ;.db $40,$1F,$E0,$1F,$A0,$1F,$20,$20,$60,$00,$20,$20,$C0,$00,$E0,$1F
        ;.db $00,$C0
        
crash0  vctr(8d,-144d,hidden)
        vctr(-16d,-40d,visible)
        vctr(88d,24d,visible)
        vctr(-24d,24d,visible)
        vctr(64d,40d,visible)
        vctr(-40d,208d,visible)
        vctr(-120d,80d,visible)
        vctr(-72d,-112d,visible)
        vctr(40d,-216d,visible)
        vctr(80d,-8d,visible)
        vctr(-8d,144d,hidden)
        rtsl
        ;.db $70,$1F,$08,$00,$D8,$1F,$F0,$3F,$18,$00,$58,$20,$34,$4C,$28,$00
        ;.db $40,$20,$D0,$00,$D8,$3F,$50,$00,$88,$3F,$90,$1F,$B8,$3F,$28,$1F
        ;.db $28,$20,$F8,$1F,$50,$20,$90,$00,$F8,$1F,$00,$C0
        
flame4  vctr(32d,-192d,hidden)
        vctr(12d,-24d,visible)
        vctr(4d,28d,visible)
        vctr(-48d,188d,hidden)
        rtsl
        ;.db $40,$1F,$20,$00,$26,$54,$22,$4E,$BC,$00,$D0,$1F,$00,$C0

flame5  vctr(24d,-192d,hidden)
        vctr(24d,-40d,visible)
        vctr(8d,48d,visible)
        vctr(-56d,184d,hidden)
        rtsl
        ;.db $40,$1F,$18,$00,$D8,$1F,$18,$20,$30,$00,$08,$20,$B8,$00,$C8,$1F
        ;.db $00,$C0
        
flame6  vctr(16d,-192d,hidden)
        vctr(36d,-60d,visible)
        vctr(12d,72d,visible)
        vctr(-64d,180d,hidden)
        rtsl
        ;.db $40,$1F,$10,$00,$C4,$1F,$24,$20,$48,$00,$0C,$20,$B4,$00,$C0,$1F
        ;.db $00,$C0
        
flame7  vctr(8d,-192d,hidden)
        vctr(48d,-88d,visible)
        vctr(16d,104d,visible)
        vctr(-72d,176d,hidden)
        rtsl
        ;.db $40,$1F,$08,$00,$A8,$1F,$30,$20,$68,$00,$10,$20,$B0,$00,$B8,$1F
        ;.db $00,$C0
        
crash1  vctr(40d,-144d,hidden)
        vctr(-8d,-40d,visible)
        vctr(88d,40d,visible)
        vctr(-32d,16d,visible)
        vctr(56d,48d,visible)
        vctr(-80d,200d,visible)
        vctr(-128d,56d,visible)
        vctr(-48d,-120d,visible)
        vctr(80d,-208d,visible)
        vctr(72d,8d,visible)
        vctr(-40d,144d,hidden)
        rtsl
        ;.db $70,$1F,$28,$00,$D8,$1F,$F8,$3F,$28,$00,$58,$20,$10,$00,$E0,$3F
        ;.db $30,$00,$38,$20,$C8,$00,$B0,$3F,$38,$00,$80,$3F,$88,$1F,$D0,$3F
        ;.db $30,$1F,$50,$20,$08,$00,$48,$20,$90,$00,$D8,$1F,$00,$C0
        
flame8  vctr(64d,-180d,hidden)
        vctr(16d,-16d,visible)
        vctr(0d,20d,visible)
        vctr(-80d,176d,hidden)
        rtsl
        ;.db $4C,$1F,$40,$00,$28,$58,$20,$4A,$B0,$00,$B0,$1F,$00,$C0

flame9  vctr(56d,-184d,hidden)
        vctr(32d,-36d,visible)
        vctr(0d,52d,visible)
        vctr(-88d,168d,hidden)
        rtsl
        ;.db $48,$1F,$38,$00,$DC,$1F,$20,$20,$34,$00,$00,$20,$A8,$00,$A8,$1F
        ;.db $00,$C0
        
flamea  vctr(48d,-188d,hidden)
        vctr(48d,-52d,visible)
        vctr(0d,76d,visible)
        vctr(-96d,164d,hidden)
        rtsl
        ;.db $44,$1F,$30,$00,$CC,$1F,$30,$20,$4C,$00,$00,$20,$A4,$00,$A0,$1F
        ;.db $00,$C0
        
flameb  vctr(40d,-192d,hidden)
        vctr(64d,-72d,visible)
        vctr(0d,100d,visible)
        vctr(-104d,164d,hidden)
        rtsl
        ;.db $40,$1F,$28,$00,$B8,$1F,$40,$20,$64,$00,$00,$20,$A4,$00,$98,$1F
        ;.db $00,$C0
        
crash2  vctr(88d,-120d,hidden)
        vctr(8d,-40d,visible)
        vctr(64d,72d,visible)
        vctr(-40d,0d,visible)
        vctr(32d,72d,visible)
        vctr(-152d,152d,visible)
        vctr(-136d,0d,visible)
        vctr(0d,-136d,visible)
        vctr(152d,-144d,visible)
        vctr(72d,32d,visible)
        vctr(-88d,120d,hidden)
        rtsl
        ;.db $88,$1F,$58,$00,$D8,$1F,$08,$20,$48,$00,$40,$20,$00,$00,$D8,$3F
        ;.db $48,$00,$20,$20,$98,$00,$68,$3F,$00,$00,$78,$3F,$78,$1F,$00,$20
        ;.db $70,$1F,$98,$20,$20,$00,$48,$20,$78,$00,$A8,$1F,$00,$C0
        
flamec  vctr(136d,-136d,hidden)
        vctr(24d,-16d,visible)
        vctr(-16d,24d,visible)
        vctr(-144d,128d,hidden)
        rtsl
        ;.db $78,$1F,$88,$00,$2C,$58,$38,$4C,$80,$00,$70,$1F,$00,$C0

flamed  vctr(128d,-144d,hidden)
        vctr(48d,-24d,visible)
        vctr(-28d,48d,visible)
        vctr(-148d,120d,hidden)
        rtsl
        ;.db $70,$1F,$80,$00,$E8,$1F,$30,$20,$30,$00,$E4,$3F,$78,$00,$6C,$1F
        ;.db $00,$C0
        
flamee  vctr(124d,-148d,hidden)
        vctr(68d,-36d,visible)
        vctr(-36d,68d,visible)
        vctr(-156d,116d,hidden)
        rtsl
        ;.db $6C,$1F,$7C,$00,$DC,$1F,$44,$20,$44,$00,$DC,$3F,$74,$00,$64,$1F
        ;.db $00,$C0
        
flamef  vctr(116d,-156d,hidden)
        vctr(92d,-44d,visible)
        vctr(-48d,96d,visible)
        vctr(-160d,104d,hidden)
        rtsl
        ;.db $64,$1F,$74,$00,$D4,$1F,$5C,$20,$60,$00,$D0,$3F,$68,$00,$60,$1F
        ;.db $00,$C0
        
crash3  vctr(144d,-24d,hidden)
        vctr(-24d,-72d,visible)
        vctr(-216d,0d,visible)
        vctr(-96d,96d,visible)
        vctr(96d,96d,visible)
        vctr(216d,0d,visible)
        vctr(24d,-72d,visible)
        vctr(32d,24d,visible)
        vctr(0d,-96d,visible)
        vctr(-32d,24d,visible)
        vctr(-144d,24d,hidden)
        rtsl
        ;.db $E8,$1F,$90,$00,$B8,$1F,$E8,$3F,$00,$00,$28,$3F,$60,$00,$A0,$3F
        ;.db $60,$00,$60,$20,$00,$00,$D8,$20,$B8,$1F,$18,$20,$18,$00,$20,$20
        ;.db $A0,$1F,$00,$20,$18,$00,$E0,$3F,$18,$00,$70,$1F,$00,$C0
        
crash4  vctr(88d,120d,hidden)
        vctr(8d,40d,visible)
        vctr(64d,-72d,visible)
        vctr(-40d,0d,visible)
        vctr(32d,-72d,visible)
        vctr(-152d,-152d,visible)
        vctr(-136d,0d,visible)
        vctr(0d,136d,visible)
        vctr(152d,144d,visible)
        vctr(72d,-32d,visible)
        vctr(-88d,-120d,hidden)
        rtsl
        ;.db $78,$00,$58,$00,$28,$00,$08,$20,$B8,$1F,$40,$20,$00,$00,$D8,$3F
        ;.db $B8,$1F,$20,$20,$68,$1F,$68,$3F,$00,$00,$78,$3F,$88,$00,$00,$20
        ;.db $90,$00,$98,$20,$E0,$1F,$48,$20,$88,$1F,$A8,$1F,$00,$C0
        
crash5  vctr(40d,144d,hidden)
        vctr(-8d,40d,visible)
        vctr(88d,-40d,visible)
        vctr(-32d,-16d,visible)
        vctr(56d,-48d,visible)
        vctr(-80d,-200d,visible)
        vctr(-128d,-56d,visible)
        vctr(-48d,120d,visible)
        vctr(80d,208d,visible)
        vctr(72d,-8d,visible)
        vctr(-40d,-144d,hidden)
        rtsl
        ;.db $90,$00,$28,$00,$28,$00,$F8,$3F,$D8,$1F,$58,$20,$F0,$1F,$E0,$3F
        ;.db $D0,$1F,$38,$20,$38,$1F,$B0,$3F,$C8,$1F,$80,$3F,$78,$00,$D0,$3F
        ;.db $D0,$00,$50,$20,$F8,$1F,$48,$20,$70,$1F,$D8,$1F,$00,$C0
        
crash6  vctr(8d,144d,hidden)
        vctr(-16d,40d,visible)
        vctr(88d,-24d,visible)
        vctr(-24d,-24d,visible)
        vctr(64d,-40d,visible)
        vctr(-40d,-208d,visible)
        vctr(-120d,-80d,visible)
        vctr(-72d,112d,visible)
        vctr(40d,216d,visible)
        vctr(80d,8d,visible)
        vctr(-8d,-144d,hidden)
        rtsl
        ;.db $90,$00,$08,$00,$28,$00,$F0,$3F,$E8,$1F,$58,$20,$34,$54,$D8,$1F
        ;.db $40,$20,$30,$1F,$D8,$3F,$B0,$1F,$88,$3F,$70,$00,$B8,$3F,$D8,$00
        ;.db $28,$20,$08,$00,$50,$20,$70,$1F,$F8,$1F,$00,$C0
        
crash7  vctr(-72d,-160d,hidden)
        vctr(-64d,80d,visible)
        vctr(-56d,-8d,visible)
        vctr(16d,32d,visible)
        vctr(0d,-16d,visible)
        vctr(32d,16d,visible)
        vctr(-16d,-16d,visible)
        vctr(16d,0d,visible)
        vctr(48d,-16d,visible)
        vctr(-8d,24d,visible)
        vctr(40d,16d,visible)
        vctr(-48d,24d,visible)
        vctr(-8d,48d,visible)
        vctr(72d,32d,visible)
        vctr(-24d,0d,hidden)
        vctr(8d,72d,visible)
        vctr(80d,8d,visible)
        vctr(-16d,32d,visible)
        vctr(32d,0d,visible)
        vctr(0d,-8d,visible)
        vctr(64d,-16d,visible)
        vctr(-32d,-32d,visible)
        vctr(72d,-32d,visible)
        vctr(8d,8d,visible)
        vctr(0d,8d,visible)
        vctr(8d,-40d,hidden)
        vctr(8d,8d,visible)
        vctr(-12d,8d,visible)
        vctr(-16d,-8d,visible)
        vctr(-20d,-72d,visible)
        vctr(-40d,-8d,visible)
        vctr(16d,-8d,hidden)
        vctr(24d,-16d,visible)
        vctr(-24d,-60d,visible)
        vctr(8d,-12d,visible)
        vctr(-8d,0d,hidden)
        vctr(-48d,-16d,visible)
        vctr(16d,-16d,visible)
        vctr(-40d,-16d,visible)
        rtsl
        ;.db $60,$1F,$B8,$1F,$50,$00,$C0,$3F,$F8,$1F,$C8,$3F,$20,$00,$10,$20
        ;.db $20,$58,$10,$00,$20,$20,$38,$58,$28,$40,$F0,$1F,$30,$20,$3C,$4C
        ;.db $10,$00,$28,$20,$18,$00,$D0,$3F,$30,$00,$F8,$3F,$20,$00,$48,$20
        ;.db $14,$40,$48,$00,$08,$20,$08,$00,$50,$20,$20,$00,$F0,$3F,$00,$00
        ;.db $20,$20,$20,$5C,$F0,$1F,$40,$20,$E0,$1F,$E0,$3F,$E0,$1F,$48,$20
        ;.db $24,$44,$20,$44,$D8,$1F,$08,$00,$24,$44,$3A,$44,$38,$5C,$B8,$1F
        ;.db $EC,$3F,$F8,$1F,$D8,$3F,$08,$5C,$2C,$58,$C4,$1F,$E8,$3F,$24,$5A
        ;.db $1C,$40,$F0,$1F,$D0,$3F,$28,$58,$F0,$1F,$D8,$3F,$00,$C0         
    .org $7FFF
    .chk $6000
    .end
    

.export tactc0,tactc1,tactc2,tactc3
.export rtarrow,uparrow,dnarrow,ltarrow,nearrow,searrow,nwarrow,swarrow
.export ltng0,ltng1,ltng2,ltng3,ltng4,ltng5,ltng6,ltng7,ltng8
.export ltng0x,ltng1x,ltng2x,ltng3x,ltng4x,ltng5x,ltng6x,ltng7x,ltng8x
.export pupl00,pupl10,pupl20,pupl30,pupl40,pupl50
.export wing00,wing01,wing02,wing03
.export wing10,wing11,wing12,wing13
.export wing20,wing21,wing22,wing23
.export wing30,wing31,wing32,wing33
.export wing40,wing41,wing42,wing43
.export wing50,wing51,wing52,wing53,bang0
.export dod0,dod1,dod2
.export ngwi0,ngwi1,ngwi2,ngwi3,ngwi4,ngwi5
.export coil0,coil1,coil2,coil3,coil4,coil5
.export wdgt0,wdgt1,wdgt2,wdgt3
.export magic0,magic1,magic2,magic3
.export bootz1,bootz2,shoes,ovhng,lock,key,escpod,sqr,dial
.export crash0,crash1,crash2,crash3,crash4,crash5,crash6,crash7
.export flame0,flame1,flame2,flame3,flame4,flame5,flame6,flame7
.export flame8,flame9,flamea,flameb,flamec,flamed,flamee,flamef
.export star0,star1,star2,star3
.export hand,box,swtch0,swtch1

.export ltg7,clock7,boot7,tite7,lock7,tran7,hand7,pod7,tacct7