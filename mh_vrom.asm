#include "vector.ah"
#include "logic.ah"
#include "a_ram.ah"

.module vrom
#include "mh_vrom0.exp"
#include "mh_vrom1.exp"
#include "mh_vrom2.exp"
#include "mh_vrom3.exp"



;********************************************
;* Major Havoc Vector ROM                   *
;********************************************
	.title "TWVectors - Vector Tables"
	
	
		.org $5000
		
_lastchk 	.equ	$

char_a		.db $20,$46,$26,$46,$26,$40,$20,$54,$14,$46,$2C,$40,$04,$5A,$00,$C0	
char_space	.db $00,$00,$20,$00,$00,$C0				
char_b		.db $20,$4C,$29,$40,$23,$5D,$20,$57,$34,$40,$00,$46,$2C,$40,$06,$E8
char_c		.db $0C,$40,$3A,$40,$3A,$46,$23,$46,$29,$40,$04,$54,$00,$C0
char_d		.db $20,$4C,$29,$40,$23,$5A,$3D,$5A,$37,$40,$00,$00,$20,$00,$00,$C0
char_e		.db $2C,$40,$14,$40
char_f		.db $20,$4C,$2C,$40,$1D,$5A,$37,$40,$F4,$1F,$20,$00,$00,$C0
char_g		.db $03,$40,$3D,$46,$23,$46,$29,$40,$1A,$5A,$26,$40,$20,$5A,$37,$40,$0D,$40,$00,$C0
char_h		.db $20,$4C,$00,$5A,$2C,$40,$00,$46,$20,$54,$04,$40,$00,$C0
char_i		.db $02,$40,$28,$40,$1C,$40,$20,$4C,$1C,$40,$28,$40,$06,$54,$00,$C0			
char_j		.db $02,$43,$26,$5D,$23,$43,$20,$49,$05,$54,$00,$C0
char_k		.db $20,$4C,$00,$5A,$29,$40,$23,$46,$1D,$5A,$23,$5A,$04,$40,$00,$C0
char_l		.db $20,$4C,$00,$54,$2C,$40,$04,$40,$00,$C0			
char_m		.db $20,$4C,$26,$5A,$26,$46,$20,$54,$04,$40,$00,$C0
char_n		.db $20,$4C,$2C,$54,$20,$4C,$04,$54,$00,$C0
char_q		.db $09,$45,$25,$5B,$12,$40
char_o		.db $09,$40,$23,$46,$3D,$46
char_o1     .db $3A,$40,$3D,$5A,$23,$5A,$26,$40,$07,$40,$00,$C0
            
char_r		.db $0C,$40,$3D,$45,$18,$5B
char_p		.db $20,$4C,$29,$40,$23,$5C,$3C,$5D,$38,$40,$F6,$1F,$20,$00,$00,$C0
char_s		.db $29,$40,$23,$43,$34,$46,$23,$43,$26,$40,$07,$54,$00,$C0
char_t		.db $06,$40,$20,$4C,$1A,$40,$2C,$40,$04,$54,$00,$C0
char_u		.db $00,$43,$20,$49,$0C,$40,$20,$57,$3A,$5D,$3A,$43,$FA,$1F,$20,$00,$00,$C0
char_v		.db $02,$4C,$28,$54,$20,$4C,$06,$54,$00,$C0
char_w		.db $00,$4C,$26,$54,$20,$4C,$26,$54,$20,$4C,$04,$54,$00,$C0
char_x		.db $2C,$4C,$14,$40,$2C,$54,$04,$40,$00,$C0
char_y		.db $00,$4C,$28,$5A,$04,$46,$E8,$1F,$F0,$3F,$0C,$40,$00,$C0
char_z		.db $00,$4C,$2C,$40,$34,$54,$2C,$40,$04,$40,$00,$C0
char_1		.db $06,$40,$20,$4C,$0A,$54,$00,$C0
char_2		.db $00,$4C,$2C,$40,$20,$5A,$34,$40,$20,$5A,$2C,$40,$04,$40,$00,$C0
char_8		.db $20,$4C,$00,$54
char_3		.db $00,$46,$2C,$40,$14,$46,$2C,$40,$20,$54,$34,$40,$00,$00,$20,$00,$00,$C0
char_9		.db $2C,$40,$00,$4C,$34,$40,$00,$54
char_4		.db $00,$4C,$20,$5A,$2C,$40,$00,$46,$20,$54,$04,$40,$00,$C0
char_6		.db $20,$46,$00,$5A
char_5		.db $2C,$40,$20,$46,$34,$40,$20,$46,$2C,$40,$04,$54,$00,$C0
char_0		.db $00,$4C,$20,$54,$2C,$40,$14,$40
char_7		.db $00,$4C,$2C,$40,$20,$54,$04,$40,$00,$C0

			jsrl(char_space)
			jsrl(char_space)
			jsrl(char_space)
			.db $1B,$60
			jsrl(havoc1)
			jsrl(char_space)
			jsrl(char_space)
			jsrl(char_space)
			jsrl(char_space)
			.db $E2,$60
			.db $00,$C0
			
char_percent	.db $00,$49,$20,$43,$23,$40,$20,$5D,$3D,$40,$0C,$43,$34,$54,$09,$40,$23,$40,$20,$43,$3D,$40,$20,$5D,$07,$40,$00,$C0
char_colon		.db $06,$40,$20,$43,$23,$40,$20,$5D,$3D,$40,$00,$46,$20,$43,$23,$40,$20,$5D,$3D,$40,$0A,$5A,$00,$C0
char_dash		.db $00,$47,$2C,$40,$04,$59,$00,$C0
char_comma		.db $12,$A9,$02,$40,$3C,$59,$0E,$46,$00,$C0
char_period		.db $12,$A9,$0C,$5F,$00,$C0
char_dot		.db $04,$41,$22,$40,$3E,$40,$00,$C0
char_excla		.db $12,$A9,$01,$43,$21,$48,$3E,$40,$21,$58,$0B,$5C,$00,$C0
char_half		.db $00,$46,$20,$46,$0C,$40,$34,$54,$0C,$40,$3D,$40,$20,$43,$23,$40,$20,$43,$3D,$40,$07,$5A,$00,$C0
			
						
sc_01			.db $00,$46,$23,$46,$26,$40,$20,$5A,$3D,$40,$03,$40,$20,$5A,$04,$40,$00,$C0
sc_02			.db $26,$40,$29,$4C,$11,$40,$26,$40,$29,$54,$04,$40,$00,$C0
sc_03			.db $00,$46,$23,$46,$29,$40,$23,$5A,$3D,$5A,$37,$40,$0D,$40,$00,$C0
sc_04			.db $00,$4C,$2C,$40,$17,$40,$20,$54,$0D,$40,$00,$C0
sc_05			.db $2C,$40,$3A,$46,$26,$46,$34,$40,$E8,$1F,$20,$00,$00,$C0
sc_07			.db $00,$46,$26,$43,$23,$5D,$3D,$5D,$3A,$43,$06,$46,$20,$54,$07,$40,$00,$C0
sc_08			.db $00,$46,$29,$46,$20,$54,$37,$46,$0D,$5A,$00,$C0
sc_09			.db $03,$40,$3D,$46,$23,$46,$1D,$5A,$2F,$40,$1D,$46,$23,$5A,$3D,$5A,$07,$40,$00,$C0
sc_10			.db $2C,$4C,$20,$54,$04,$40,$00,$C0
sc_11			.db $20,$4C,$2C,$54,$1A,$46,$26,$46,$04,$54,$00,$C0
sc_12			.db $26,$46,$1A,$46,$2C,$54,$04,$40,$00,$C0
sc_13			.db $00,$46,$26,$46,$2C,$54,$00,$4C,$34,$54,$3A,$46,$F4
sc_14			.db $1F,$2C,$00,$00,$C0
sc_15			.db $20,$4C,$2C,$40,$20,$54,$34,$40,$06,$46,$00,$00,$00,$20,$09,$5A,$00,$C0
sc_16			.db $26,$46,$1A,$46,$26,$5A,$26,$40,$06,$E8
sc_17			.db $26,$4C,$26,$54,$04,$40,$00,$C0	
sc_18			.db $23,$40,$20,$4C,$2C,$54,$20,$4C,$23,$40,$04,$54,$00,$C0
sc_19			.db $00,$46,$26,$46,$26,$5A,$3A,$5A,$3A,$46,$F4,$1F,$20,$00,$00,$C0
sc_20			.db $20,$4C,$00,$5A,$29,$40,$06,$E8
sc_21			.db $20,$46,$23,$46,$26,$54,$23,$46,$06,$E8
sc_22			.db $20,$4C,$26,$40,$03,$40,$20,$5A,$37,$40,$0C,$5A,$00,$C0
sc_23			.db $23,$46,$20,$46,$03,$40,$20,$54,$03,$40,$26,$46,$20,$46,$07,$54,$00,$C0
sc_24			.db $06,$40,$3A,$46,$26,$46,$20,$5A,$26,$40,$06,$E8
sc_25			.db $00,$46,$23,$40,$3D,$46,$29,$40,$20,$54,$04,$40,$00,$C0
sc_26			.db $00,$4C,$23,$40,$20,$57,$29,$40,$00,$46,$3D,$40,$03,$5D,$3D,$40,$00,$43,$20,$57,$07,$40,$00,$C0
sc_27			.db $23,$46,$3D,$46,$03,$40,$23,$5A,$3D,$5A,$07,$40,$00,$C0
sc_28			.db $2C,$4C,$14,$40,$2C,$54,$04,$40,$00,$C0
sc_29			.db $20,$46,$26,$46,$26,$5A,$20,$5A,$04,$40,$00,$C0
sc_30			.db $03,$40,$3D,$40,$20,$4C,$2C,$40,$20,$54,$3D,$40,$07,$40,$00,$C0
sc_31			.db $00,$4C,$26,$54,$00,$4C,$26,$54,$1D,$43,$29,$49,$04,$54,$00,$C0
sc_32			.db $20,$46,$00,$46,$29,$54,$00,$46,$20,$46,$04,$54,$00,$C0
sc_33			.db $2C,$4C,$34,$40,$2C,$54,$34,$40,$00,$00,$20,$00,$00,$C0
			
lifech		.db $01,$40,$23,$4C,$3C,$5C,$22,$46,$03,$00,$06,$20,$01,$00,$FE,$3F
			.db $05,$00,$00,$20,$22,$41,$22,$5F,$FB,$1F,$00,$20,$FF,$1F,$FE,$3F
			.db $FD,$1F,$06,$20,$22,$5A,$3C,$44,$23,$54,$00,$00,$F9,$3F,$0C,$00
			.db $FD,$3F,$F4,$1F,$FD,$3F,$00,$00,$F9,$3F,$0F,$40,$00,$C0
			
qmark		.db $06,$40,$22,$40,$20,$42,$3E,$40,$20,$5E,$00,$44,$22,$40,$20,$42
			.db $24,$44,$20,$44,$3C,$44,$3C,$40,$3C,$5C,$22,$5E,$22,$42,$24,$40
			.db $20,$5C,$3E,$5C,$20,$5E,$00,$C0
			
vgmsga
vgjchsp		jsrl(char_space)
vgjch0		jsrl(char_0)
vgjch1		jsrl(char_1)
vgjch2		jsrl(char_2)
vgjch3		jsrl(char_3)
vgjch4		jsrl(char_4)
vgjch5		jsrl(char_5)
vgjch6		jsrl(char_6)
vgjch7		jsrl(char_7)
vgjch8		jsrl(char_8)
vgjch9		jsrl(char_9)
vgjcha		jsrl(char_a)
vgjchb		jsrl(char_b)
vgjchc		jsrl(char_c)
vgjchd		jsrl(char_d)
vgjche		jsrl(char_e)
vgjchf		jsrl(char_f)
vgjchg		jsrl(char_g)
vgjchh		jsrl(char_h)
vgjchi		jsrl(char_i)
vgjchj		jsrl(char_j)
vgjchk		jsrl(char_k)
vgjchl		jsrl(char_l)
vgjchm		jsrl(char_m)
vgjchn		jsrl(char_n)
vgjcho		jsrl(char_o)
vgjchp		jsrl(char_p)
vgjchq		jsrl(char_q)
vgjchr		jsrl(char_r)
vgjchs		jsrl(char_s)
vgjcht		jsrl(char_t)
formsg
vgjchu		jsrl(char_u)
vgjchv		jsrl(char_v)
vgjchw		jsrl(char_w)
vgjchx		jsrl(char_x)
vgjchy		jsrl(char_y)
vgjchz		jsrl(char_z)
			jsrl(char_period)
			jsrl(char_period)
			jsrl(char_excla)
			jsrl(char_dash)
			jsrl(char_comma)
			jsrl(char_percent)
			jsrl(char_colon)
			jsrl(char_half)
			rtsl            ;this is here to end the alpha-numerics for the crosshatch test pattern
			jsrl(sc_01)
			jsrl(sc_02)
			jsrl(sc_03)
			jsrl(sc_04)
			jsrl(sc_05)
			jsrl(sc_07)
			jsrl(sc_08)
			jsrl(sc_09)
			jsrl(sc_10)
			jsrl(sc_11)
			jsrl(sc_12)
			jsrl(sc_13)
			jsrl(sc_15)
			jsrl(sc_16)
			jsrl(sc_17)
			jsrl(sc_18)
			jsrl(sc_19)
			jsrl(patch)
			jsrl(sc_21)
			jsrl(sc_22)
			jsrl(sc_23)
			jsrl(sc_24)
			jsrl(sc_25)
			jsrl(sc_26)
			jsrl(sc_27)
			jsrl(sc_28)
			jsrl(sc_29)
			jsrl(sc_30)
			jsrl(sc_31)
			jsrl(sc_32)
			jsrl(sc_33)
			jsrl(char_l)
			rtsl
			
onearw		vctr(-44d,28d,visible)
            vctr(44d,28d,visible)
            vctr(124d,0d,visible)
            vctr(-28d,-28d,visible)
            vctr(28d,-28d,visible)
            vctr(-124d,0d,visible)
            rtsl
            ;.db $1C,$00,$D4,$3F,$1C,$00,$2C,$20,$00,$00,$7C,$20,$32,$52,$2E,$52,$00,$00,$84,$3F,$00,$C0
            
onevln		vctr(0d,48d,visible)
            vctr(24d,-48d,hidden)
            vctr(0d,48d,visible)
            rtsl
            ;.db $30,$00,$00,$20,$D0,$1F,$18,$00,$30,$00,$00,$20,$00,$C0
       
onearln     vctr(-4d,-128d,hidden)
            jsrl(onevln) ;$551E
            vctr(-60d,0d,hidden)
            jsrl(onearw)    ;$5508
            vctr(0d,104d,hidden)
            jsrl(onearw)    ;$5508
            vctr(36d,56d,hidden)
            jmpl(onevln)                       
			;.db $80,$1F,$FC,$1F,$8F,$AA,$00,$00,$C4,$1F,$84,$AA
			;.db $68,$00,$00,$00,$84,$AA,$38,$00,$24,$00,$8F,$EA
            
onesigt		jsrl(char_o)
            jsrl(char_n)
            jsrl(char_e)
            vctr(-88d,-104d,hidden)
            jsrl(char_w)
            jsrl(char_a)
            jmpl(char_y)
            ;.db $65,$A8,$5D,$A8,$22,$A8,$98,$1F,$A8,$1F,$94,$A8,$00,$A8,$A0,$E8

onesigr		jsrl(onearln)   ;Arrows and Vert Lines
            vctr(-64d,-88d,hidden)
            rtsl
            ;.db $96,$AA,$A8,$1F,$C0,$1F,$00,$C0

onesigl		jsrl(onearln)
            vctr(24d,-88d,hidden)
            rtsl
            ;.db $96,$AA,$A8,$1F,$18,$00,$00,$C0
			
crosshatch	vstat(sparkle_off,xflip_off,vpage0,$C,colwhite)
            vctr(-1024d,0d,hidden)
            vctr(768d,-864d,visible)
            vctr(256d,288d,visible)
            vctr(-512d,576d,visible)
            vctr(-512d,-576d,visible)
            vctr(256d,-288d,visible)
            vctr(768d,864d,visible)
            vctr(0d,-864d,hidden)
            vctr(-768d,864d,visible)
            vctr(-256d,-288d,visible)
            vctr(512d,-576d,visible)
            vctr(512d,576d,visible)
            vctr(-256d,288d,visible)
            vctr(-768d,-864d,visible)
            vcntr
            vscal(ywin_off,binscal2,$20)
            vctr(-600d,-128d,hidden)
            jmpl(vgmsga)            ;Shows the Alpha Numerics
            
            vctr(1023d,0d,visible)
            vctr(0d,-2d,visible)
            vctr(-1024d,0d,visible)
            vctr(0d,-2d,visible)
            rtsl

            ;.db $C7,$60,$00,$00,$00,$1C,$A0,$1C,$00,$23,$20,$01,$00,$21,$40,$02
			;.db $00,$3E,$C0,$1D,$00,$3E,$E0,$1E,$00,$21,$60,$03,$00,$23,$A0,$1C
			;.db $00,$00,$60,$03,$00,$3D,$E0,$1E,$00,$3F,$C0,$1D,$00,$22,$40,$02
			;.db $00,$22,$20,$01,$00,$3F,$A0,$1C,$00,$3D,$20,$80,$20,$72,$80,$1F
			;.db $A8,$1D,$35,$EA
			
			;.db $00,$00,$FF,$23,$20,$5F,$00,$00,$00,$3C,$20,$5F,$00,$C0
			
mazet		jsrl(maze1)
			jsrl(maze2)
			jsrl(maze3)
			jsrl(maze4)
			jsrl(maze5)
			jsrl(maze6)
			jsrl(maze7)

mapet		jsrl(mape1)
			jsrl(mape2)
			jsrl(mape3)
			jsrl(mape4)
			jsrl(mape5)
			jsrl(mape6)
			jsrl(mape7)

mansrc		jsrl(pic0)
			jsrl(pic1)
			jsrl(pic2)
			jsrl(pic3)
			jsrl(pic4)
			jsrl(pic5)
			jsrl(pic6)
			jsrl(pic7)
			jsrl(pic8)
			jsrl(pic9)
			jsrl(pic10)
			jsrl(pic11)
			jsrl(pic12)
			jsrl(pic13)
			jsrl(pic14)
			jsrl(pic15)
			jsrl(pic16)
			jsrl(pic17)
			jsrl(pic18)
			jsrl(pic19)
			jsrl(pic20)
			jsrl(pic21)
			jsrl(pic22)
			jsrl(pic23)
			jsrl(pic24)
			jsrl(pic25)
			jsrl(pic26)
			jsrl(pic27)
			jsrl(pic28)
			jsrl(pic29)
			jsrl(pic30)
			jsrl(pic31)
			jsrl(pic32)
			jsrl(pic33)
			jsrl(pic34)
			jsrl(pic35)
			jsrl(pic36)
			jsrl(pic37)
			jsrl(pic38)
			jsrl(pic39)
			jsrl(pic40)
			jsrl(vecram)
			
rods		jsrl(rod0)
			jsrl(rod1)
			jsrl(rod2)
			jsrl(rod3)
			
heads		jsrl(head0)
			jsrl(head0)
			jsrl(head1)
			jsrl(head2)
			jsrl(head3)
			jsrl(head4)
			jsrl(head5)
			jsrl(head6)
			
tails		jsrl(tail0)
			jsrl(tail1)

guns		jsrl(gun0)
			jsrl(gun1)
			jsrl(gun2)
			jsrl(gun3)

eyes		jsrl(eye0)
			jsrl(eye1)
			jsrl(eye2)

cann		jsrl(mount)
			jsrl(lgun0)
			jsrl(lgun1)
			jsrl(lgun2)
			jsrl(lgun3)
			jsrl(brl00)
			jsrl(brl10)
			jsrl(brl20)
			jsrl(brl30)
			jsrl(brl01)
			jsrl(brl11)
			jsrl(brl21)
			jsrl(brl31)
			jsrl(brl02)
			jsrl(brl12)
			jsrl(brl22)
			jsrl(brl32)
			jsrl(laz00)
			jsrl(laz10)
			jsrl(laz20)
			jsrl(laz30)
			jsrl(laz01)
			jsrl(laz10)
			jsrl(laz20)
			jsrl(laz31)
			jsrl(laz02)
			jsrl(laz10)
			jsrl(laz20)
			jsrl(laz32)
			jsrl(laz03)
			jsrl(laz10)
			jsrl(laz20)
			jsrl(laz33)
			
maxheads    jsrl(robothead0)
            jsrl(robothead1)
            jsrl(robothead2)
            jsrl(robothead3)
            jsrl(robothead4)
            jsrl(robothead5)
            jsrl(robothead6)
            jsrl(robothead7)
            
maxbods     jsrl(robotbody0)
            jsrl(robotbody1)
            jsrl(robotbody2)
            jsrl(robotbody3)
            jsrl(robotbody4)
            jsrl(robotbody5)
            jsrl(robotbody6)
            jsrl(robotbody7)
            
lvts		jsrl(levt0)
		    jsrl(levt1)
			jsrl(levt2)
			jsrl(levt3)

planes		jsrl(plne1)
			jsrl(plne2)
			jsrl(plne3)
			jsrl(plne4)
			jsrl(plne5)
			jsrl(plne6)
			jsrl(plne8)
			jsrl(plne9)
			jsrl(plne10)
			jsrl(plne11)
			jsrl(plne12)
			jsrl(plne13)
			jsrl(plne15)
			jsrl(plne16)
			jsrl(plne17)
			jsrl(plne18)
			jsrl(plne19)
			jsrl(plne20)
			jsrl(plne21)
			jsrl(plne22)
			jsrl(plne23)
			jsrl(plne24)
			jsrl(plne25)
			jsrl(plne26)

			
beapic		jsrl(beacn0)
			jsrl(beacn1)
			jsrl(beacn2)
			jsrl(beacn3)
			jsrl(beacn4)
			jsrl(beacn5)
			jsrl(beacn6)
			jsrl(beacn7)

movet		jsrl(t31)
			jsrl(t30)
			jsrl(t29)
			jsrl(t28)
			jsrl(t27)
			jsrl(t26)
			jsrl(t25)
			jsrl(t24)
			jsrl(t23)
			jsrl(t22)
			jsrl(t21)
			jsrl(t20)
			jsrl(t19)
			jsrl(t18)
			jsrl(t17)
			jsrl(t16)
			jsrl(t15)
			jsrl(t14)
			jsrl(t13)
			jsrl(t12)
			jsrl(t11)
			jsrl(t10)
			jsrl(t9)
			jsrl(t8)
			jsrl(t7)
			jsrl(t6)
			jsrl(t5)
			jsrl(t4)
			jsrl(t3)
			jsrl(t2)
			jsrl(t1)
			jsrl(t0)
						
bases		jsrl(st0)
			jsrl(st3)
			jsrl(st2)
			jsrl(st1)

fbase		jsrl(ltlsh)
			jsrl(hexsh)
			jsrl(ftrsh)
			jsrl(dmbsh)

enemys		jsrl(enmy0)
			jsrl(enmy1)
			jsrl(enmy2)
			jsrl(enmy3)
			jsrl(enmy4)
			jsrl(enmy5)
			jsrl(enmy6)
			jsrl(enmy7)
			jsrl(enmy8)
			
shtexp		jsrl(exp9)
			jsrl(exp10)
			jsrl(exp11)
			jsrl(exp12)
			jsrl(exp13)
			jsrl(exp14)
			jsrl(exp15)
			jsrl(exp16)

fexps		jsrl(fexp1)
			jsrl(fexp2)
			jsrl(fexp3)
			jsrl(fexp4)
			jsrl(fexp5)
			jsrl(fexp6)
			jsrl(fexp7)
			jsrl(fexp8)

bxp0s		jsrl(bx00)
			jsrl(bx01)
			jsrl(bx02)
			jsrl(bx03)
			jsrl(bx04)
			jsrl(bx05)
			jsrl(bx06)
			jsrl(bx07)
			
bxp1s		jsrl(bx10)
			jsrl(bx11)
			jsrl(bx12)
			jsrl(bx13)
			jsrl(bx14)
			jsrl(bx15)
			jsrl(bx16)
			jsrl(bx17)

sexps		jsrl(sexp0)
			jsrl(sexp1)
			jsrl(sexp2)
			jsrl(sexp3)
			jsrl(sexp4)

sxp0s		jsrl(pc00)
			jsrl(pc01)
			jsrl(pc02)
			jsrl(pc03)
			jsrl(pc04)
			jsrl(pc05)
			jsrl(pc06)
			jsrl(pc07)

sxp1s		jsrl(pc10)
			jsrl(pc11)
			jsrl(pc12)
			jsrl(pc13)
			jsrl(pc14)
			jsrl(pc15)
			jsrl(pc16)
			jsrl(pc17)

sxp2s		jsrl(pc20)
			jsrl(pc21)
			jsrl(pc22)
			jsrl(pc23)
			jsrl(pc24)
			jsrl(pc25)
			jsrl(pc26)
			jsrl(pc27)

sxp3s		jsrl(pc30)
			jsrl(pc31)
			jsrl(pc32)
			jsrl(pc33)
			jsrl(pc34)
			jsrl(pc35)
			jsrl(pc36)
			jsrl(pc37)

sxps		.word sxp0s
			.word sxp1s
			.word sxp2s
			.word sxp3s

smtb		jsrl(smtb0)
			jsrl(smtb1)
			jsrl(smtb2)
			jsrl(smtb3)

tcn			jsrl(tactc0)
			jsrl(tactc1)
			jsrl(tactc3)
			jsrl(tactc2)

mazarw		jsrl(rtarrow)
			jsrl(ltarrow)
			jsrl(uparrow)
			jsrl(dnarrow)
			jsrl(nearrow)
			jsrl(swarrow)
			jsrl(nwarrow)
			jsrl(searrow)
			jsrl(qmark)
			jsrl(outrw0)
			jsrl(outrw1)
			jsrl(outrw2)
			jsrl(outrw3)
			jsrl(outwrd)
					
lightning	jsrl(ltng0)
			jsrl(ltng1)
			jsrl(ltng2)
			jsrl(ltng3)
			jsrl(ltng4)
			jsrl(ltng5)
			jsrl(ltng6)
			jsrl(ltng7)
			jsrl(ltng8)
			
			jsrl(ltng0x)
			jsrl(ltng1x)
			jsrl(ltng2x)
			jsrl(ltng3x)
			jsrl(ltng4x)
			jsrl(ltng5x)
			jsrl(ltng6x)
			jsrl(ltng7x)
			jsrl(ltng8x)
			
cerpup		jsrl(pupl00)
			jsrl(pupl10)
			jsrl(pupl20)
			jsrl(pupl30)
			jsrl(pupl40)
			jsrl(pupl50)

cerwng		jsrl(wing00)
			jsrl(wing01)
			jsrl(wing02)
			jsrl(wing03)
			jsrl(wing10)
			jsrl(wing11)
			jsrl(wing12)
			jsrl(wing13)
			jsrl(wing20)
			jsrl(wing21)
			jsrl(wing22)
			jsrl(wing23)
			jsrl(wing30)
			jsrl(wing31)
			jsrl(wing32)
			jsrl(wing33)
			jsrl(wing40)
			jsrl(wing41)
			jsrl(wing42)
			jsrl(wing43)
			jsrl(wing50)
			jsrl(wing51)
			jsrl(wing52)
			jsrl(wing53)
			
onesign		jsrl(onesigr)
			jsrl(onesigl)
			jsrl(onesigt)

newshot		jsrl(nwsht0)
			jsrl(nwsht1)
			jsrl(nwsht2)
			jsrl(nwsht3)
			jsrl(nwsht4)
			jsrl(nwsht5)
			jsrl(nwsht6)
			jsrl(nwsht7)

cerstf		jsrl(dod0)
			jsrl(dod1)
			jsrl(dod2)
			
cerbng		jsrl(bang0)
			jsrl(ngwi0)
			jsrl(ngwi1)
			jsrl(ngwi2)
			jsrl(ngwi3)
			jsrl(ngwi4)
			jsrl(ngwi5)
			jsrl(coil0)
			jsrl(coil1)
			jsrl(coil2)
			jsrl(coil3)
			jsrl(coil4)
			jsrl(coil5)
			jsrl(wdgt0)
			jsrl(wdgt1)
			jsrl(wdgt2)
			jsrl(wdgt3)

gclock		jsrl(sqr)
			jsrl(dial)

gboot		jsrl(shoes)
			jsrl(magic0)
			jsrl(magic1)
			jsrl(magic2)
			jsrl(magic3)
			jsrl(bootz1)
			jsrl(bootz2)

gtite		jsrl(ovhng)

glock		jsrl(lock)
gkey		jsrl(key)

gpod		jsrl(epod1)
			jsrl(epod2)
			jsrl(epod3)
			jsrl(epod4)
			jsrl(epod5)
			jsrl(epod6)
			jsrl(escpod)
			jsrl(crash0)
			
			jsrl(crash1)
			jsrl(crash2)
			jsrl(crash3)
			jsrl(crash4)
			jsrl(crash5)
			jsrl(crash6)
			jsrl(crash7)
			jsrl(flame0)
			jsrl(flame1)
			jsrl(flame2)
			jsrl(flame3)
			jsrl(flame4)
			jsrl(flame5)
			jsrl(flame6)
			jsrl(flame7)
			jsrl(flame8)
			jsrl(flame9)
			jsrl(flamea)
			jsrl(flameb)
			jsrl(flamec)
			jsrl(flamed)
			jsrl(flamee)
			jsrl(flamef)
	
			
gtran		jsrl(booth3)
			jsrl(booth4)
			jsrl(booth5)
			jsrl(star0)
			jsrl(star1)
			jsrl(star2)
			jsrl(star3)

ghand		jsrl(hand)
			jsrl(box)
			jsrl(swtch0)
			jsrl(swtch1)
            
;*********************************************
;* End lookup Tables... Start RAW Data       *
;*********************************************

booth3		vctr(0d,-36d,hidden)
            vctr(4d,4d,visible)
            vctr(0d,64d,visible)
            vctr(-4d,4d,visible)
            vctr(4d,0d,visible)
            vctr(4d,-4d,visible)
            vctr(0d,-64d,visible)
            vctr(-4d,-4d,visible)
            vctr(-48d,0d,visible)
            vctr(0d,-4d,visible)
            vctr(4d,-4d,visible)
            vctr(56d,0d,visible)
            vctr(4d,4d,visible)
            vctr(0d,4d,visible)
            vctr(-2d,4d,visible)
            vctr(0d,64d,visible)
            vctr(2d,4d,visible)
            vctr(0d,4d,visible)
            vctr(-4d,4d,visible)
            vctr(-56d,0d,visible)
            vctr(-4d,-4d,visible)
            vctr(0d,-4d,visible)
            vctr(44d,0d,visible)
            rtsl
            ;.db $DC,$1F,$00,$00,$22,$42,$40,$00,$00,$20,$3E,$42,$22,$40,$22,$5E
			;.db $C0,$1F,$00,$20,$3E,$5E,$00,$00,$D0,$3F,$20,$5E,$22,$5E,$00,$00
			;.db $38,$20,$22,$42,$20,$42,$3F,$42,$40,$00,$00,$20,$21,$42,$20,$42
			;.db $3E,$42,$00,$00,$C8,$3F,$3E,$5E,$20,$5E,$00,$00,$2C,$20,$00,$C0

booth4		vctr(-4d,-4d,visible)
            vctr(-32d,0d,visible)
            vctr(-4d,4d,visible)
            vctr(0d,-72d,hidden)
            vctr(4d,4d,visible)
            vctr(32d,0d,visible)
            vctr(4d,-4d,visible)
            rtsl
            ;.db $3E,$5E,$00,$00,$E0,$3F,$3E,$42,$B8,$1F,$00,$00,$22,$42,$00,$00
			;.db $20,$20,$22,$5E,$00,$C0
			
booth5		vctr(10d,0d,hidden)
            jsrl(boothdot)
            vctr(0d,75d,hidden)
            jsrl(boothdot)
            vctr(-30d,-39d,hidden)
            rtsl
            ;.db $05,$40,$C5,$AC,$4B,$00,$00,$00,$C5,$AC,$D9,$1F,$E2,$1F,$00,$C0
            
boothdot	vctr(3d,2d,visible)
            vctr(3d,-2d,visible)
            vctr(0d,-3d,visible)
            vctr(-3d,-2d,visible)
            vctr(-3d,2d,visible)
            vctr(0d,3d,visible)
            rtsl
            ;.db $02,$00,$03,$20,$FE,$1F,$03,$20,$FD,$1F,$00,$20,$FE,$1F,$FD,$3F
			;.db $02,$00,$FD,$3F,$03,$00,$00,$20,$00,$C0
			
epod6		vctr(56d,-104d,hidden)
            jsrl(char_e)
            vctr(-88d,104d,hidden)
            ;.db $98,$1F,$38,$00,$22,$A8,$68,$00,$A8,$1F
epod5	    vctr(56d,-72d,hidden)
            jsrl(char_p)
            vctr(-88d,72d,hidden)
            ;.db $B8,$1F,$38,$00,$71,$A8,$48,$00,$A8,$1F
epod4		vctr(56d,-40d,hidden)
            jsrl(char_a)
            vctr(-88d,40d,hidden)
            ;.db $D8,$1F,$38,$00,$00,$A8,$28,$00,$A8,$1F
epod3		vctr(56d,-8d,hidden)
            jsrl(char_c)
            vctr(-88d,8d,hidden)
            ;.db $F8,$1F,$38,$00,$13,$A8,$08,$00,$A8,$1F
epod2		vctr(56d,24d,hidden)
            jsrl(char_s)
            vctr(-88d,-24d,hidden)
            ;.db $18,$00,$38,$00,$79,$A8,$E8,$1F,$A8,$1F
epod1		vctr(56d,56d,hidden)
            jsrl(char_e)
            vctr(-88d,-56d,hidden)
            rtsl
            ;.db $38,$00,$38,$00,$22,$A8,$C8,$1F,$A8,$1F,$00,$C0
	
havoc1		vctr(240d,-22d,hidden)
            vctr(-8d,-14d,visible)
            vctr(-176d,0d,visible)
            vctr(12d,28d,visible)
            vctr(-16d,0d,visible)
            vctr(-12d,-28d,visible)
            vctr(-214d,0d,visible)
            vctr(18d,44d,visible)
            vctr(-28d,-44d,visible)
            vctr(-22d,0d,visible)
            vctr(10d,44d,visible)
            vctr(-22d,-44d,visible)
            vctr(-20d,0d,visible)
            vctr(38d,64d,visible)
            vctr(20d,0d,visible)
            vctr(-8d,-48d,visible)
            vctr(32d,48d,visible)
            vctr(20d,0d,visible)
            vctr(-20d,-50d,visible)
            vctr(186d,0d,visible)
            vctr(22d,50d,visible)
            vctr(16d,0d,visible)
            vctr(-10d,-24d,visible)
            vctr(16d,0d,visible)
            vctr(10d,24d,visible)
            vctr(16d,0d,visible)
            vctr(-22d,-50d,visible)
            vctr(162d,0d,visible)
            vctr(-240d,22d,hidden)
            rtsl
            ;.db $EA,$1F,$F0,$00,$3C,$59,$00,$00,$50,$3F,$26,$4E,$38,$40,$3A,$52
			;.db $00,$00,$2A,$3F,$2C,$00,$12,$20,$D4,$1F,$E4,$3F,$35,$40,$2C,$00
			;.db $0A,$20,$D4,$1F,$EA,$3F,$36,$40,$40,$00,$26,$20,$2A,$40,$D0,$1F
			;.db $F8,$3F,$30,$00,$20,$20,$2A,$40,$CE,$1F,$EC,$3F,$00,$00,$BA,$20
			;.db $32,$00,$16,$20,$28,$40,$3B,$54,$28,$40,$25,$4C,$28,$40,$CE,$1F
			;.db $EA,$3F,$00,$00,$A2,$20,$16,$00,$10,$1F,$00,$C0
			
havoc2		vctr(-128d,-8d,hidden)
            vctr(4d,0d,visible)
            vctr(8d,16d,visible)
            vctr(-24d,-28d,visible)
            vctr(-12d,0d,visible)
            vctr(32d,40d,visible)
            vctr(20d,0d,visible)
            vctr(-12d,-40d,visible)
            vctr(-20d,0d,visible)
            vctr(28d,8d,hidden)
            vctr(4d,12d,visible)
            vctr(12d,0d,visible)
            vctr(-4d,-8d,visible)
            vctr(8d,0d,visible)
            vctr(12d,28d,visible)
            vctr(12d,0d,visible)
            vctr(-16d,-40d,visible)
            vctr(-24d,0d,visible)
            vctr(-4d,8d,visible)
            vctr(48d,32d,hidden)
            vctr(32d,0d,visible)
            vctr(-16d,-40d,visible)
            vctr(-32d,0d,visible)
            vctr(16d,40d,visible)
            vctr(8d,-12d,hidden)
            vctr(8d,0d,visible)
            vctr(-8d,-16d,visible)
            vctr(-8d,0d,visible)
            vctr(8d,16d,visible)
            vctr(28d,-28d,hidden)
            vctr(-14d,0d,visible)
            vctr(16d,40d,visible)
            vctr(34d,0d,visible)
            vctr(4d,-8d,visible)
            vctr(-16d,-12d,visible)
            vctr(12d,-20d,visible)
            vctr(-16d,0d,visible)
            vctr(-10d,20d,visible)
            vctr(10d,8d,visible)
            vctr(-8d,0d,visible)
            vctr(-12d,-28d,visible)
            vctr(128d,12d,hidden)
            vctr(4d,0d,visible)
            vctr(8d,16d,visible)
            vctr(-20d,-28d,visible)
            vctr(-14d,0d,visible)
            vctr(30d,40d,visible)
            vctr(20d,0d,visible)
            vctr(-12d,-40d,visible)
            vctr(-20d,0d,visible)
            vctr(36d,40d,hidden)
            vctr(14d,0d,visible)
            vctr(-8d,-28d,visible)
            vctr(22d,32d,visible)
            vctr(14d,0d,visible)
            vctr(-30d,-44d,visible)
            vctr(-24d,0d,visible)
            vctr(12d,40d,visible)
            vctr(40d,0d,hidden)
            vctr(32d,0d,visible)
            vctr(-16d,-40d,visible)
            vctr(-32d,0d,visible)
            vctr(16d,40d,visible)
            vctr(8d,-12d,hidden)
            vctr(8d,0d,visible)
            vctr(-8d,-16d,visible)
            vctr(-8d,0d,visible)
            vctr(8d,16d,visible)
            vctr(12d,-28d,hidden)
            vctr(16d,40d,visible)
            vctr(32d,0d,visible)
            vctr(-6d,-16d,visible)
            vctr(-12d,0d,visible)
            vctr(2d,4d,visible)
            vctr(-8d,0d,visible)
            vctr(-8d,-16d,visible)
            vctr(8d,0d,visible)
            vctr(2d,4d,visible)
            vctr(12d,0d,visible)
            vctr(-6d,-16d,visible)
            vctr(-32d,0d,visible)
            vctr(-196d,20d,hidden)
            rtsl
            ;.db $F8,$1F,$80,$1F,$22,$40,$24,$48,$34,$52,$3A,$40,$28,$00,$20,$20
			;.db $2A,$40,$D8,$1F,$F4,$3F,$36,$40,$0E,$44,$22,$46,$26,$40,$3E,$5C
			;.db $24,$40,$26,$4E,$26,$40,$D8,$1F,$F0,$3F,$34,$40,$3E,$44,$20,$00
			;.db $30,$00,$00,$00,$20,$20,$D8,$1F,$F0,$3F,$00,$00,$E0,$3F,$28,$00
			;.db $10,$20,$04,$5A,$24,$40,$3C,$58,$3C,$40,$24,$48,$0E,$52,$39,$40
			;.db $28,$00,$10,$20,$00,$00,$22,$20,$22,$5C,$38,$5A,$26,$56,$38,$40
			;.db $3B,$4A,$25,$44,$3C,$40,$3A,$52,$0C,$00,$80,$00,$22,$40,$24,$48
			;.db $36,$52,$39,$40,$28,$00,$1E,$20,$2A,$40,$D8,$1F,$F4,$3F,$36,$40
			;.db $28,$00,$24,$00,$27,$40,$3C,$52,$20,$00,$16,$20,$27,$40,$D4,$1F
			;.db $E2,$3F,$34,$40,$28,$00,$0C,$20,$00,$00,$28,$00,$00,$00,$20,$20
			;.db $D8,$1F,$F0,$3F,$00,$00,$E0,$3F,$28,$00,$10,$20,$04,$5A,$24,$40
			;.db $3C,$58,$3C,$40,$24,$48,$06,$52,$28,$00,$10,$20,$00,$00,$20,$20
			;.db $3D,$58,$3A,$40,$21,$42,$3C,$40,$3C,$58,$24,$40,$21,$42,$26,$40
			;.db $3D,$58,$00,$00,$E0,$3F,$14,$00,$3C,$1F,$00,$C0
			
havoc3		vctr(-228d,-36d,hidden)
            vctr(32d,56d,visible)
            vctr(8d,0d,visible)
            vctr(-8d,-48d,visible)
            vctr(8d,0d,visible)
            vctr(32d,48d,visible)
            vctr(8d,0d,visible)
            vctr(-16d,-48d,visible)
            vctr(200d,0d,visible)
            vctr(24d,56d,visible)
            vctr(-14d,-30d,hidden)
            vctr(32d,0d,visible)
            vctr(14d,30d,hidden)
            vctr(-24d,-56d,visible)
            vctr(168d,0d,visible)
            vctr(-236d,28d,hidden)
            rtsl
            ;.db $DC,$1F,$1C,$1F,$38,$00,$20,$20,$24,$40,$D0,$1F,$F8,$3F,$24,$40
			;.db $30,$00,$20,$20,$24,$40,$D0,$1F,$F0,$3F,$00,$00,$C8,$20,$38,$00
			;.db $18,$20,$19,$51,$00,$00,$20,$20,$07,$4F,$C8,$1F,$E8,$3F,$00,$00
			;.db $A8,$20,$1C,$00,$14,$1F,$00,$C0
			
glint0		rtsl ;.db $00,$C0
glint1		vctr(-200d,28d,hidden)
            rtsl
            ;.db $1C,$00,$38,$1F,$00,$C0
glint2		vctr(-184d,-36d,hidden)
            rtsl
            ;.db $DC,$1F,$48,$1F,$00,$C0
glint3		vctr(-120d,20d,hidden)
            rtsl
            ;.db $14,$00,$88,$1F,$00,$C0
glint4		vctr(-124d,-8d,hidden)
            rtsl
            ;.db $F8,$1F,$84,$1F,$00,$C0
glint5		vctr(-80d,-20d,hidden)
            rtsl
            ;.db $EC,$1F,$B0,$1F,$00,$C0
glint6		vctr(-52d,12d,hidden)
            rtsl
            ;.db $0C,$00,$CC,$1F,$00,$C0
glint7		vctr(-48d,-36d,hidden)
            rtsl
            ;.db $DC,$1F,$D0,$1F,$00,$C0
glint8		vctr(84d,28d,hidden)
            rtsl
            ;.db $1C,$00,$54,$00,$00,$C0
glint9		vctr(96d,-36d,hidden)
            rtsl
            ;.db $DC,$1F,$60,$00,$00,$C0
glinta		vctr(116d,8d,hidden)
            rtsl
            ;.db $08,$00,$74,$00,$00,$C0
glintb		vctr(120d,-20d,hidden)
            rtsl
            ;.db $EC,$1F,$78,$00,$00,$C0
glintc		vctr(148d,20d,hidden)
            rtsl
            ;.db $14,$00,$94,$00,$00,$C0
glintd		vctr(184d,-8d,hidden)
            rtsl
            ;.db $F8,$1F,$B8,$00,$00,$C0
glinte		vctr(244d,20d,hidden)
            rtsl
            ;.db $14,$00,$F4,$00,$00,$C0
glintf		vctr(232d,-36d,hidden)
            rtsl
            ;.db $DC,$1F,$E8,$00,$00,$C0

crman		vscal(ywin_off,binscal1,$20)
            vstat(sparkle_off,xflip_off,vpage2,$F,colcyan)
            jsrl(mapdot)
            vcntr
            vscal(ywin_off,binscal2,$00)
            rtsl
            ;.db $20,$71,$F3,$62,$69,$B0,$20,$80,$00,$72,$00,$C0
            
crreac		vscal(ywin_off,binscal1,$20)
            vstat(sparkle_off,xflip_off,vpage2,$F,colorange)
            jsrl(mapdot)
            vcntr
            vscal(ywin_off,binscal2,$00)
            rtsl
            ;.db $20,$71,$FA,$62,$69,$B0,$20,$80,$00,$72,$00,$C0
            
crfire		vscal(ywin_off,binscal1,$80)
            vstat(sparkle_off,xflip_off,vpage2,$F,colwhiter)
            jsrl(mapdot)
            vcntr
            vscal(ywin_off,binscal2,$00)
            rtsl
            ;.db $80,$71,$F8,$62,$69,$B0,$20,$80,$00,$72,$00,$C0
            
crlsht		vscal(ywin_off,binscal1,$80)
            vstat(sparkle_off,xflip_off,vpage2,$4,colredr)
            jsrl(mapdot)
            vcntr
            vscal(ywin_off,binscal2,$00)
            rtsl
            ;.db $80,$71,$4B,$62,$69,$B0,$20,$80,$00,$72,$00,$C0
            
crcann		vscal(ywin_off,binscal1,$40)
            vstat(sparkle_off,xflip_off,vpage2,$C,colwhite)
            jsrl(mapdot)
            vcntr
            vscal(ywin_off,binscal2,$00)
            rtsl
            ;.db $40,$71,$C7,$62,$69,$B0,$20,$80,$00,$72,$00,$C0
            
crrob		vscal(ywin_off,binscal1,$80)
            vstat(sparkle_off,xflip_off,vpage2,$E,colbluer)
            jsrl(mapdot)
            vcntr
            vscal(ywin_off,binscal2,$00)
            rtsl
            ;.db $80,$71,$EE,$62,$69,$B0,$20,$80,$00,$72,$00,$C0
;crmax		vscal(ywin_off,binscal1,$80)
;            vstat(sparkle_off,xflip_off,vpage2,$8,colredr)
;            jsrl(mapdot)
;            vcntr
;            vscal(ywin_off,binscal2,$00)
;            rtsl
            ;.db $80,$71,$8B,$62,$69,$B0,$20,$80,$00,$72,$00,$C0

longline	vctr(608d,0d,7)
            rtsl
            ;.db $00,$00,$60,$E2,$00,$C0

tact0		vctr(-32d,0d,hidden)
            vctr(9d,24d,visible)
            vctr(10d,8d,visible)
            vctr(25d,0d,visible)
            vctr(9d,-8d,visible)
            vctr(11d,-24d,visible)
            vctr(-16d,-20d,visible)
            vctr(-32d,0d,visible)
            vctr(-16d,20d,visible)
            vctr(64d,0d,visible)
            vctr(-16d,20d,visible)
            vctr(-32d,0d,visible)
            vctr(-16d,-20d,visible)
            vctr(16d,19d,hidden)
            vctr(3d,13d,visible)
            vctr(29d,-12d,hidden)
            vctr(-4d,12d,visible)
            vctr(-44d,40d,hidden)
            vctr(6d,-4d,visible)
            vctr(0d,-8d,visible)
            vctr(-6d,-4d,visible)
            vctr(-8d,4d,visible)
            vctr(0d,8d,visible)
            vctr(8d,4d,visible)
            vctr(64d,0d,hidden)
            vctr(7d,-4d,visible)
            vctr(0d,-8d,visible)
            vctr(-7d,-4d,visible)
            vctr(-8d,4d,visible)
            vctr(0d,8d,visible)
            vctr(8d,4d,visible)
            vctr(64d,-56d,hidden)
            vctr(7d,-4d,visible)
            vctr(0d,-8d,visible)
            vctr(-7d,-4d,visible)
            vctr(-7d,3d,visible)
            vctr(0d,9d,visible)
            vctr(7d,4d,visible)
            vctr(-48d,-73d,hidden)
            vctr(7d,-4d,visible)
            vctr(0d,-8d,visible)
            vctr(-7d,-4d,visible)
            vctr(-7d,4d,visible)
            vctr(0d,8d,visible)
            vctr(7d,4d,visible)
            vctr(-96d,0d,hidden)
            vctr(6d,-4d,visible)
            vctr(0d,-8d,visible)
            vctr(-7d,-4d,visible)
            vctr(-7d,4d,visible)
            vctr(0d,8d,visible)
            vctr(8d,4d,visible)
            vctr(-49d,73d,hidden)
            vctr(7d,-4d,visible)
            vctr(0d,-9d,visible)
            vctr(-7d,-3d,visible)
            vctr(-8d,3d,visible)
            vctr(1d,9d,visible)
            vctr(7d,4d,visible)
            vctr(97d,-16d,hidden)
            rtsl
            
            ;.db $00,$00,$E0,$1F,$18,$00,$09,$20,$25,$44,$00,$00,$19,$20,$F8,$1F
			;.db $09,$20,$E8,$1F,$0B,$20,$38,$56,$00,$00,$E0,$3F,$38,$4A,$00,$00
			;.db $40,$20,$38,$4A,$00,$00,$E0,$3F,$38,$56,$13,$00,$10,$00,$0D,$00
			;.db $03,$20,$F4,$1F,$1D,$00,$3E,$46,$28,$00,$D4,$1F,$23,$5E,$20,$5C
			;.db $3D,$5E,$3C,$42,$20,$44,$24,$42,$00,$00,$40,$00,$FC,$1F,$07,$20
			;.db $20,$5C,$FC,$1F,$F9,$3F,$3C,$42,$20,$44,$24,$42,$C8,$1F,$40,$00
			;.db $FC,$1F,$07,$20,$20,$5C,$FC,$1F,$F9,$3F,$03,$00,$F9,$3F,$09,$00
			;.db $00,$20,$04,$00,$07,$20,$B7,$1F,$D0,$1F,$FC,$1F,$07,$20,$20,$5C
			;.db $FC,$1F,$F9,$3F,$04,$00,$F9,$3F,$20,$44,$04,$00,$07,$20,$00,$00
			;.db $A0,$1F,$23,$5E,$20,$5C,$FC,$1F,$F9,$3F,$04,$00,$F9,$3F,$20,$44
			;.db $24,$42,$49,$00,$CF,$1F,$FC,$1F,$07,$20,$F7,$1F,$00,$20,$FD,$1F
			;.db $F9,$3F,$03,$00,$F8,$3F,$09,$00,$01,$20,$04,$00,$07,$20,$F0,$1F
			;.db $61,$00,$00,$C0
			
tact1		vctr(24d,6d,visible)
            vctr(10d,-6d,visible)
            vctr(52d,14d,visible)
            vctr(0d,50d,visible)
            vctr(-20d,18d,visible)
            vctr(-42d,8d,visible)
            vctr(-48d,0d,visible)
            vctr(-42d,-8d,visible)
            vctr(-20d,-18d,visible)
            vctr(0d,-50d,visible)
            vctr(52d,-14d,visible)
            vctr(10d,6d,visible)
            vctr(24d,-6d,visible)
            vctr(0d,42d,visible)
            vctr(86d,22d,visible)
            vctr(-54d,18d,visible)
            vctr(-32d,-8d,visible)
            vctr(-32d,8d,visible)
            vctr(-54d,-18d,visible)
            vctr(86d,-22d,visible)
            vctr(0d,20d,hidden)
            vctr(18d,4d,visible)
            vctr(6d,8d,visible)
            vctr(-24d,-6d,visible)
            vctr(-24d,6d,visible)
            vctr(6d,-8d,visible)
            vctr(18d,-4d,visible)
            vctr(32d,20d,hidden)
            vctr(-8d,8d,visible)
            vctr(-48d,0d,hidden)
            vctr(-8d,-8d,visible)
            vctr(16d,-104d,hidden)
            vctr(-32d,0d,visible)
            vctr(14d,-12d,visible)
            vctr(18d,12d,visible)
            vctr(32d,0d,hidden)
            vctr(32d,0d,visible)
            vctr(-14d,-12d,visible)
            vctr(-18d,12d,visible)
            vctr(0d,-32d,hidden)
            vctr(-32d,0d,visible)
            vctr(14d,-12d,visible)
            vctr(18d,12d,visible)
            vctr(-16d,54d,hidden)
            rtsl
            ;.db $2C,$43,$25,$5D,$0E,$00,$34,$20,$32,$00,$00,$20,$36,$49,$08,$00
			;.db $D6,$3F,$00,$00,$D0,$3F,$F8,$1F,$D6,$3F,$36,$57,$CE,$1F,$00,$20
			;.db $F2,$1F,$34,$20,$25,$43,$2C,$5D,$2A,$00,$00,$20,$16,$00,$56,$20
			;.db $12,$00,$CA,$3F,$F8,$1F,$E0,$3F,$08,$00,$E0,$3F,$EE,$1F,$CA,$3F
			;.db $EA,$1F,$56,$20,$00,$4A,$29,$42,$23,$44,$34,$5D,$34,$43,$23,$5C
			;.db $29,$5E,$14,$00,$20,$00,$3C,$44,$00,$00,$D0,$1F,$3C,$5C,$98,$1F
			;.db $10,$00,$00,$00,$E0,$3F,$27,$5A,$29,$46,$00,$00,$20,$00,$00,$00
			;.db $20,$20,$39,$5A,$37,$46,$E0,$1F,$00,$00,$00,$00,$E0,$3F,$27,$5A
			;.db $29,$46,$36,$00,$F0,$1F,$00,$C0
			
tact2		vctr(-32d,-22d,hidden)
            vctr(-21d,24d,visible)
            vctr(0d,30d,visible)
            vctr(30d,18d,visible)
            vctr(45d,0d,visible)
            vctr(30d,-18d,visible)
            vctr(0d,-30d,visible)
            vctr(-20d,-24d,visible)
            vctr(-64d,0d,visible)
            vctr(16d,-4d,visible)
            vctr(32d,0d,visible)
            vctr(16d,4d,visible)
            vctr(0d,34d,visible)
            vctr(20d,20d,visible)
            vctr(-30d,12d,visible)
            vctr(-6d,-6d,visible)
            vctr(16d,-26d,visible)
            vctr(-65d,0d,visible)
            vctr(1d,-34d,visible)
            vctr(17d,72d,hidden)
            vctr(-8d,-7d,visible)
            vctr(-30d,-11d,visible)
            vctr(20d,-20d,visible)
            vctr(17d,26d,visible)
            vctr(-7d,5d,visible)
            vctr(6d,-5d,hidden)
            vctr(33d,0d,visible)
            vctr(6d,6d,hidden)
            vctr(-8d,6d,visible)
            vctr(-14d,-51d,hidden)
            vctr(4d,-5d,visible)
            vctr(-4d,-3d,visible)
            vctr(-4d,4d,visible)
            vctr(4d,4d,visible)
            vctr(0d,1d,hidden)
            rtsl
            ;.db $EA,$1F,$E0,$1F,$18,$00,$EB,$3F,$20,$4F,$2F,$49,$00,$00,$2D,$20
			;.db $2F,$57,$20,$51,$36,$54,$00,$00,$C0,$3F,$28,$5E,$00,$00,$20,$20
			;.db $28,$42,$22,$00,$00,$20,$2A,$4A,$31,$46,$3D,$5D,$28,$53,$00,$00
			;.db $BF,$3F,$DE,$1F,$01,$20,$48,$00,$11,$00,$F9,$1F,$F8,$3F,$F5,$1F
			;.db $E2,$3F,$2A,$56,$1A,$00,$11,$20,$05,$00,$F9,$3F,$FB,$1F,$06,$00
			;.db $00,$00,$21,$20,$03,$43,$3C,$43,$CD,$1F,$F2,$1F,$FB,$1F,$04,$20
			;.db $FD,$1F,$FC,$3F,$3E,$42,$22,$42,$01,$00,$00,$00,$00,$C0
			
tact3		vctr(-42d,0d,hidden)
            vctr(-6d,-32d,visible)
            vctr(-64d,0d,visible)
            vctr(80d,144d,visible)
            vctr(18d,0d,visible)
            vctr(-10d,-40d,visible)
            vctr(42d,0d,visible)
            vctr(6d,-32d,visible)
            vctr(-24d,0d,visible)
            vctr(0d,32d,visible)
            vctr(30d,0d,hidden)
            vctr(16d,0d,visible)
            vctr(12d,-32d,visible)
            vctr(-18d,0d,visible)
            vctr(-10d,32d,visible)
            vctr(-6d,14d,hidden)
            vctr(-24d,0d,visible)
            vctr(0d,26d,visible)
            vctr(32d,0d,visible)
            vctr(80d,-144d,visible)
            vctr(-128d,0d,visible)
            vctr(4d,26d,hidden)
            vctr(2d,28d,visible)
            vctr(-26d,0d,visible)
            vctr(4d,18d,visible)
            vctr(-40d,0d,visible)
            vctr(84d,-46d,hidden)
            vctr(-2d,28d,visible)
            vctr(72d,0d,visible)
            vctr(-24d,-10d,hidden)
            vctr(16d,-44d,visible)
            vctr(-36d,132d,hidden)
            vctr(-24d,0d,visible)
            vctr(-14d,-100d,hidden)
            rtsl
            ;.db $00,$00,$D6,$1F,$E0,$1F,$FA,$3F,$00,$00,$C0,$3F,$90,$00,$50,$20
			;.db $29,$40,$D8,$1F,$F6,$3F,$00,$00,$2A,$20,$E0,$1F,$06,$20,$34,$40
			;.db $20,$00,$00,$20,$0F,$40,$28,$40,$E0,$1F,$0C,$20,$37,$40,$20,$00
			;.db $F6,$3F,$1D,$47,$34,$40,$20,$4D,$00,$00,$20,$20,$70,$1F,$50,$20
			;.db $00,$00,$80,$3F,$02,$4D,$21,$4E,$33,$40,$22,$49,$00,$00,$D8,$3F
			;.db $D2,$1F,$54,$00,$3F,$4E,$00,$00,$48,$20,$14,$5B,$D4,$1F,$10,$20
			;.db $84,$00,$DC,$1F,$34,$40,$9C,$1F,$F2,$1F,$00,$C0
			
scan0		vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            rtsl
            ;.db $22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$22,$40
			;.db $22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$22,$40
			;.db $22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$22,$40
			;.db $22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$00,$C0
			
scan1		vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            rtsl
            ;.db $22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$22,$46,$22,$54
			;.db $22,$49,$22,$5D,$22,$40,$22,$40,$22,$40,$22,$40,$22,$40,$22,$46
			;.db $22,$54,$22,$49,$22,$5D,$22,$40,$22,$40,$22,$40,$22,$40,$22,$40
			;.db $22,$46,$22,$54,$22,$49,$22,$5D,$22,$40,$22,$40,$00,$C0
			
scan2		vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            rtsl
            ;.db $22,$40,$22,$40,$22,$46,$22,$54,$22,$49,$22,$5D,$22,$40,$22,$40
			;.db $22,$46,$22,$54,$22,$49,$22,$5D,$22,$40,$22,$40,$22,$46,$22,$54
			;.db $22,$49,$22,$5D,$22,$40,$22,$40,$22,$46,$22,$54,$22,$49,$22,$5D
			;.db $22,$40,$22,$40,$22,$46,$22,$54,$22,$49,$22,$5D,$00,$C0
			
scan3		vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            vctr(4d,0d,visible)
            vctr(4d,12d,visible)
            vctr(4d,-24d,visible)
            vctr(4d,18d,visible)
            vctr(4d,-6d,visible)
            rtsl
            ;.db $22,$40,$22,$46,$22,$54,$22,$49,$22,$5D,$22,$40,$22,$46,$22,$54
			;.db $22,$49,$22,$5D,$22,$40,$22,$46,$22,$54,$22,$49,$22,$5D,$22,$40
			;.db $22,$46,$22,$54,$22,$49,$22,$5D,$22,$40,$22,$46,$22,$54,$22,$49
			;.db $22,$5D,$22,$40,$22,$46,$22,$54,$22,$49,$22,$5D,$00,$C0
			
;***********************************
	.sbttl "Spot Killer"
;***********************************
spot7 = $60
		
spot		vcntr
            vscal(ywin_off,binscal2,$00)
            vctr(-300d,-300d,hidden)
            vctr(600d,600d,hidden)
            vcntr
            rtsl
            ;.db $20,$80,$00,$72,$D4,$1E,$D4,$1E,$58,$02,$58,$02,$20,$80,$00,$C0

clline		vctr(-256d,16d,hidden)
            rtsl
            ;.db $10,$00,$00,$1F,$00,$C0
			
clpat       vctr(256d,0d,7)
            jsrl(clline)
clpt2       vctr(256d,0d,6)
            jsrl(clline)
            vctr(256d,0d,5)
            jsrl(clline)
            vctr(256d,0d,4)
            jsrl(clline)
            vctr(256d,0d,3)
            jsrl(clline)
            vctr(256d,0d,2)
            rtsl

;clpat		.db $00,$00,$00,$E1,$95,$AF
;clpt2		.db $00,$00,$00,$C1,$95,$AF
;   		.db $00,$00,$00,$A1,$95,$AF
;			.db $00,$00,$00,$81,$95,$AF
;			.db $00,$00,$00,$61,$95,$AF
;			.db $00,$00,$00,$41,$00,$C0
			
cl73		vstat(sparkle_off,xflip_off,vpage0,$1,colwhite)
            vctr(64d,0d,2)
            vctr(-320d,16d,hidden)
            vctr(256d,0d,1)
            rtsl
            ;.db $17,$60,$00,$00,$40,$40,$10,$00,$C0,$1E,$00,$00,$00,$21,$00,$C0
	
frcfl       vcntr
            vscal(ywin_off,binscal2,$00)
frbox       vctr(512d,432d,hidden)
frbx2       vctr(-1024d,0d,visible)
            vctr(0d,-864d,visible)
            vctr(1024d,0d,visible)
            vctr(0d,864d,visible)
            rtsl

;frcfl		.db $20,$80,$00,$72
;frbox		.db $B0,$01,$00,$02
;frbx2		.db $00,$00,$00,$3C,$A0,$1C,$00,$20,$00,$00,$00,$24,$60,$03,$00,$20,$00,$C0


vline		vctr(0d,-864d,visible)
            vcntr
            rtsl
            ;.db $A0,$1C,$00,$20,$20,$80,$00,$C0
			
hline		vctr(1023d,0d,visible)
            vcntr
            rtsl
            ;.db $00,$00,$FF,$23,$20,$80,$00,$C0
			
hystr		vcntr
            vscal(ywin_off,binscal1,$00)
            vctr(0d,192d,hidden)
            jsrl(hystr2)
            vctr(10d,0d,visible)
            vcntr
            vctr(0d,-192d,hidden)
            jsrl(hystr2)
            vctr(10d,0d,visible)
            vcntr
            vctr(256d,0d,hidden)
            jsrl(hystr2)
            vctr(0d,10d,visible)
            vcntr
            vctr(-256d,0d,hidden)
            jsrl(hystr2)
            vctr(0d,10d,visible)
            vcntr
            rtsl
            ;.db $20,$80,$00,$71,$C0,$00,$00,$00,$DE,$AF,$25,$40,$20,$80,$40,$1F
			;.db $00,$00,$DE,$AF,$25,$40,$20,$80,$00,$00,$00,$01,$DE,$AF,$20,$45
			;.db $20,$80,$00,$00,$00,$1F,$DE,$AF,$20,$45,$20,$80,$00,$C0
			
hystr2  	vstat(sparkle_off,xflip_off,vpage0,$8,colwhite)
            vcntr
            vcntr
            rtsl
            ;.db $87,$60,$20,$80,$20,$80,$00,$C0

waste		vstat(sparkle_off,xflip_off,vpage0,$0,colblack)
            jsrl(frcfl)
            vhalt
            ;.db $00,$60,$B2,$AF,$00,$20

patch		vscal(ywin_off,binscal1,$60)
            jsrl(char_o)
            vctr(-27d,6d,hidden)
            vscal(ywin_off,binscal2,$60)
            jsrl(char_c)
            vscal(ywin_off,binscal2,$30)
            vctr(16d,-6d,hidden)
            rtsl
            ;.db $60,$71,$65,$A8,$06,$00,$E5,$1F,$60,$72,$13,$A8,$30,$72,$08,$5D
			;.db $00,$C0

		
	
	.nocodes		;So we dont have list file buffer overflows
	.fill $7000-*
    
    .org $5FFF
    .chk $5000
	.end
	
;**************************************
;* Main VROM exports	
.export char_a,char_space,char_b,char_c,char_d,char_e,char_f,char_g,char_h,char_i,char_j,char_k,char_l
.export char_m,char_n,char_q,char_o,char_r,char_p,char_s,char_t,char_u,char_v,char_w,char_x,char_y,char_z
.export char_1,char_2,char_8,char_3,char_9,char_4,char_6,char_5,char_0,char_7,char_percent,char_colon,char_dash
.export char_comma,char_period,char_dash,char_excla,char_half,lifech,qmark,vgmsga,formsg,onearw,onesigt,onesigr,onesigl,crosshatch
.export mazet,mapet,mansrc,rods,heads,tails,guns,eyes,cann,planes,beapic,movet,bases,fbase,enemys
.export shtexp,fexps,bxp0s,bxp1s,sexps,sxp0s,sxp1s,sxp2s,sxp3s,sxps,smtb,tcn,mazarw,lightning,cerpup,cerwng,onesign
.export newshot,cerstf,cerbng,gclock,gkey,gboot,gtite,glock,gpod,gtran,ghand,booth3,booth4,booth5,epod6,epod5,epod4
.export epod3,epod2,epod1,havoc1,havoc2,havoc3,glint0,glint1,glint2,glint3,glint4,glint5,glint6,glint7
.export glint8,glint9,glinta,glintb,glintc,glintd,glinte,glintf,crman,crreac,crfire,crlsht,crcann,crrob,longline
.export tact0,tact1,tact2,tact3,scan0,scan1,scan2,scan3,spot,clpat,clpt2,cl73,frcfl,frbox,frbx2,vline,hline,hystr,waste,patch

;Levitations removed - not used
;.export lvts
;**************************************
; Character JSRL exports
.export vgjch0,vgjch1,vgjch2,vgjch3,vgjch4,vgjch5,vgjch6,vgjch7,vgjch8,vgjch9,vgjcha,vgjchb,vgjchc,vgjchd,vgjche,vgjchf
.export vgjchg,vgjchh,vgjchi,vgjchj,vgjchk,vgjchl,vgjchm,vgjchn,vgjcho,vgjchp,vgjchq,vgjchr,vgjchs,vgjcht,vgjchu,vgjchv
.export vgjchw,vgjchx,vgjchy,vgjchz,vgjchsp

;**************************************
;* VROM Page 0 exports
.export body,bodyt

;**************************************
;* VROM Page 1 exports
.export shield,pic28,pic34,leg1,leg2,brickp,padlep,brline,lrsrbx,tactd

;**************************************
;* VROM Page 2 exports
.export mapdot,shipsh
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

.export webln00,webln01,webln02,webln03,webln04,webln05,webln06
.export webln07,webln08,webln09,webln0a,webln0b

.export weblnh0,weblnh1,weblnh2,weblnh3,weblnh4,weblnh5,weblnh6
.export weblnh7,weblnh8,weblnh9,weblnha,weblnhb

.export cerexp0,cerexp1,cerexp2,cerexp3,cerexp4,cerexp5,cerexp6,cerexp7

;******** Stat ROM Page Variables **************
.export plane7,fexps7,sexps7,tube7,st7,enm7,bas7,shld7,mpic7
.export tact7,brick7,shtex7,smtb7,live7,body7,rods7,clock7,boot7
.export hand7,tite7,lock7,pod7,tran7,ltg7,mapdt7,shpsh7,spot7
.export becn7,mzls7,gun7,maz7,lshot7,tacct7