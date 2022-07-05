 .locallabelchar "?"
#include "logic.ah"
#include "vector.ah"
#include "mh_vrom.exp"
#include "mh_alpha.exp"
 .module aux3
;********************************************
;* Major Havoc Auxiliary Program Page 3     *
;********************************************
	.title "TWTEXT - Text Routines"
	
	.org $2000

cksuma	.byte $96

;**********************************************
	.sbttl "Message Routine"
;**********************************************
;* Will output specified message to specified *
;* location on screen.                        *
;*                                            *
;* Inputs: X=message @ * 2, xcomp+2, +3=Y pos *
;**********************************************
msg3		stx	temp4
		ldy	temp4				;Set up pointer to litera3
		lda	(litra3,Y)
		sta	temp5
		iny	
		lda	(litra3,Y)
		sta	temp5+1			;Set literal pointer
		ldy	#00
		sty	vgbrit			;Position dark
		lda	(temp5,Y)			;Get H position
		sta	xcomp				;X LSB/4 position
		lda	#-1
		sta	xcomp+1
		asl	xcomp
		rol	xcomp+1			;Multiply by 4
		asl	xcomp
		rol	xcomp+1			;X position
		lda	#02
		jsr	vgsca1			;Set position scale
		jsr	vgcntr
		ldx	#$27
		jsr	vgvctr			;Position
		ldx	temp4
		lda	msglbs3,X
		pha	
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		ora	#$F0
		tay	
		lda	#00
		jsr	vgstat			;Set color
		pla	
		and	#$0F
		clc	
		adc	#01
		ldy	#$5C				
		jsr	vgscal			;Set Scale
		ldy	#01
		lda	#00				;C init vglist offset
		sta	temp1
		begin
			lda	(temp5,Y)			;C get character representation
			sta	temp2
			and	#$7F
			iny	
			sty	temp3				;Save Y
			tax	
			lda	vgmsga,X			;C get correct JSRL
			bit	foreign			;C Want foreign letters?
			ifmi
				lda	formsg,X			;C Get alt char set letters
			endif
			ldy	temp1
			sta	(vglist,Y)
			iny	
			lda	vgmsga+1,X
			bit	foreign			;D Want foreign letters?
			ifmi
				lda	formsg+1,X			;C Get alt char set letters
			endif
			sta	(vglist,Y)
			iny	
			sty	temp1				;Save Y
			ldy	temp3				;C Get character ptr
			bit	temp2				;D If not end of string
		miend
		ldy	temp1				;C Update vglist
		dey	
		jmp	vgadd
		
;***********************************************
	.sbttl "Story Output"
;***********************************************
stor3 	lda	strtst			;Story status on??
		ifmi					;yep!
			lda	#00
			sec	
			adc	strtyl
			tay					;Save LSB	
			lda	strtyh
			adc	#00
			ifpl
				inc	strtln
				inc	strtln			;By 2's
				tax					;Save this too
				tya	
				sec	
				sbc	#$40				;Distance between lines
				tay	
				txa	
				sbc	#00
			endif
			sta	strtyh
			sta	xcomp+3			;Will be Y MSB
			sty	strtyl
			sty	xcomp+2			;Is LSB
			ldx	#06				;Will do 9 lines
			stx	temp9
			lda	strtln			;First line to output
			cmp	#lstmsg3			;Done all?
			ifcs
				lda	#$C0				;Change story modes
				sta	strtst			;Clear status
				lda	#-2
				sta	strtyl
				sta	strtyh
				lda	#00
				sta	strtln
				rts					;Return
			endif
			sta	temp8
			begin
				ldx	temp8
				cpx	#lstmsg3			;All on screen??
				bcs	done3
				jsr	msg3				;Do this line
				lda	temp8
				tax	
				lda	msglbs3+1,X			;Picture with this one??
				ifne					;yep
					jsr	dopic				;Do this pic
				endif
				inc	temp8
				inc	temp8				;To next line
				lda	xcomp+2			;Move down some space
				sec	
				sbc	#$40
				sta	xcomp+2
				ifcc
					dec	xcomp+3			;To next line
				endif
				dec	temp9				;Any more lines??
			miend
		endif
done3 	rts

;*********************************************
	.sbttl "Picture Draw Routines"
;*********************************************	
picjsr	.word apic1-1		;Dummy call
		.word apic1-1
		.word apic2-1
		.word apic3-1
		.word apic4-1
		.word apic5-1
		.word apic6-1
		.word apic7-1
		.word apic8-1
picjse	;End of table


dopic		asl	A
		tax					;Get the routine
		cpx	#picjse-picjsr		;Off end of table??
		ifcc					;nope!
			lda	picjsr+1,X
			pha
			lda	picjsr,X
			pha
		endif
		rts
		
;**********************************************
	.sbttl "Pic1 - Fish Bombs"
;**********************************************	
apic1		lda	#$40
		ldx	#$73
		jsr	vgadd2			;Scale
		lda	#$F7
		ldx	#$63				;Stat instruction
		jsr	vgadd2
		lda	cerpup
		ldx	cerpup+1
		jsr	vgadd2
		lda	#$F4
		ldx	#$63
		jsr	vgadd2
		lda	frame
		and	#06
		tay	
		lda	cerwng,Y
		ldx	cerwng+1,Y			;Add wings
		jmp	vgadd2
		
;**********************************************
	.sbttl "Pic2 - Fighters"
;**********************************************	
apic2		lda	#00
		ldx	#$74
		jsr	vgadd2			;Scale
		lda	#$F2
		ldx	#enm7				;Stat for fighter
		jsr	vgadd2
		lda	frame
		lsr	A
		lsr	A
		and	#$0F
		tax	
		lda	fightlist,X			;Get the pic
		tay	
		lda	enemys,Y
		ldx	enemys+1,Y			;Get enemy pic
		jmp	vgadd2
		
fightlist	.byte 0,2,4,6,8,$A,$C,$E,$E,$C,$A,8,6,4,2,0

;**********************************************
	.sbttl "Pic3 - Mazoids"
;**********************************************
apic3		lda	#$40
		ldx	#$74
		jsr	vgadd2
		lda	#$F3
		ldx	#$62
		jsr	vgadd2
		lda	frame
		lsr	A
		lsr	A
		and	#06
		tay	
		lda	mazelist,Y
		ldx	mazelist+1,Y
		jmp	vgadd2
	
;*** This table stolen from one of marks routines ***	
mazelist	.word $BE38,$BE5B,$BE7E,$BEA1

;**********************************************
	.sbttl "Pic4 - Fireballs"
;**********************************************
apic4		lda	#$F7
		ldx	#$60				;Draw a fire ball
		jsr	vgadd2
		lda	#$40
		ldx	#$72
		jsr	vgadd2
		laljsr(sparkb)
		lxhjsr(sparkb)			;Draw the fire ball	
		jmp	vgadd2
		
;**********************************************
	.sbttl "Pic5 - Robots"
;**********************************************
apic5		lda	#$40
		ldx	#$72
		jsr	vgadd2
		lda	#robcol+$E0
		ldx	#body7			;Stat and color
		jsr	vgadd2
		laljsr(body)
		lxhjsr(body)
		jsr	vgadd2			;Output body
		lda	frame
		lsr	A
		lsr	A
		and	#$0F
		ifeq
			lda	#01				;Don't let go 0
		endif
		sta	temp3				;Save for below
		cmp	#08
		ifcs
			lda	#08
		endif
		asl	A
		tay	
		lda	heads-2,Y
		ldx	heads-2+1,Y			;Put a head on it
		jsr	vgadd2
		lda	tails
		ldx	tails+1
		jsr	vgadd2			;Add a tail
		lda	#mancol2+$E0
		ldx	#body7
		jsr	vgadd2			;Color for gun
		lda	temp3				;Recall status
		sec	
		sbc	#04
		ifpl
			cmp	#04
			ifcs
				lda	#03
			endif
			asl	A
			tay
			lda	guns,Y
			ldx	guns+1,Y
			jsr	vgadd2
		endif
		lda	temp3
		cmp	#03
		ifcs
			lda	#02
		endif
		asl	A
		tay	
		lda	eyes-2,Y
		ldx	eyes-2+1,Y
		jmp	vgadd2
		
;**********************************************
	.sbttl "Pic6 - Reactoid"
;**********************************************
apic6		lda	#00
		ldx	#$72
		jsr	vgadd2
		lda	#$AA
		ldx	#body7+xflip
		jsr	vgadd2
		laljsr(body)
		lxhjsr(body)
		jsr	vgadd2
		lda	#$AA
		ldx	#rods7+xflip
		jsr	vgadd2
		lda	rods
		ldx	rods+1
		jsr	vgadd2
		lda	#$F6
		ldx	#$60
		jsr	vgadd2
		lda	#00
		ldx	#$73
		jsr	vgadd2
		laljsr(sparkb)
		lxhjsr(sparkb)
		jmp	vgadd2

;**********************************************
	.sbttl "Pic7 - Oxoids"
;**********************************************
apic7		lda	#$F5
apic72	ldx	#shld7
		jsr	vgadd2
		lda	#00
		ldx	#$75
		jsr	vgadd2
		laljsr(shield)
		lxhjsr(shield)
		jsr	vgadd2
		lda	#00
		ldx	#$73
		jsr	vgadd2
		lda	#$D0
		ldx	#$1F
		jsr	vgadd2				;1st half of a vector
		lda	#$20
		ldx	#00					;Second half of vector
		jsr	vgadd2
		lda	vgmsga+6
		ldx	vgmsga+7
		jmp	vgadd2
		
;**********************************************
	.sbttl "Pic8 - Oxoids after Reactor"
;**********************************************
apic8		lda	#$F6
		jmp	apic72
		

;**********************************************
	.sbttl "TWMSG3 - Message Tables #3"
;**********************************************
lstmsg3    = 0
___msnum3 = 0
___nmsgs3 = 35d
___eng3 = $
___csy3 = ___eng3+(___nmsgs3*2)
engmsg3 = ___eng3
msglbs3 = ___csy3

#define 	mess3(xlit3,xcol3,xsca3,xypo3)  \ .org ___eng3
#defcont   \lstmsg3 .set lstmsg3+2
#defcont   \ .word xlit3
#defcont   \___eng3 .set ___eng3+2
#defcont   \m+xlit3 = ___msnum3
#defcont   \___msnum3 .set ___msnum3+2
#defcont   \ .org ___csy3
#defcont   \ .byte ((xcol3*$10)|(xsca3))
#defcont   \ .byte xypo3
#defcont   \___csy3 .set ___csy3+2
		
		
	mess3(sco0,colcyan,1,0)		;Enemy list
	mess3(sco1,colgreen,1,0)		
	mess3(sco2,colwhite,1,0)
	mess3(sco2a,colred,1,0)
	mess3(sco3,colyellow,1,1)
	mess3(sco4,colyellow,1,2)
	mess3(sco5,colyellow,1,3)
	mess3(sco6,colred,1,0)
	mess3(sco7,colyellow,1,4)
	mess3(sco8,colyellow,1,5)
	mess3(sco9,colwhite,1,0)
	mess3(sco10,colred,1,0)
	mess3(sco11,colyellow,1,6)
	mess3(sco12,colyellow,1,7)	
	mess3(sco13,colyellow,1,8)
	mess3(sco13a,colwhite,1,0)
	mess3(sco14,colred,1,0)
	mess3(sco15,colyellow,1,0)
	mess3(sco16,colyellow,1,0)
	mess3(sco17,colyellow,1,0)
	mess3(sco18,colyellow,1,0)
	mess3(sco19,colwhite,1,0)
	mess3(sco20,colred,1,0)
	mess3(sco21,colcyan,1,0)
	mess3(sco22,colcyan,1,0)
	mess3(sco23,colcyan,1,0)
	mess3(sco24,colcyan,1,0)
	mess3(sco25,colcyan,1,0)
	mess3(sco26,colred,1,0)
	mess3(sco27,colred,1,0)
	
		;"THE ENEMY LIST"
sco0		.byte $D6,$3C,$24,$1E,$00,$1E,$30,$1E,$2E,$46,$00,$2C,$26,$3A,$BC
		
		;"   NAME          POINTS           "
sco1		.byte $9A,$00,$00,$00,$30,$16,$2E,$1E,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$34,$32,$26,$30,$3C,$3A,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$80
		
		;"                                   "
sco2		.byte $80,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C
		.byte $4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C
		.byte $4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$CC
		
		;"SPACE ENEMIES"
sco2a		.byte $D9,$3A,$34,$16,$1A,$1E,$00,$1E,$30,$1E,$2E,$26,$1E,$BA
		
		;"   FISHOIDS         1000           "
sco3		.byte $9A,$00,$00,$20,$26,$3A,$24,$32,$26,$1C,$3A,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$04,$02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$80
		
		;"  FLYBOIDS          500           "
sco4		.byte $9A,$00,$00,$20,$2C,$46,$18,$32,$26,$1C,$3A,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$0C,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$80
		
		;"   MAZOIDS          500           "
sco5		.byte $9A,$00,$00,$00,$2E,$16,$48,$32,$26,$1C,$3A,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$0C,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$80
		
		;"MAZE ENEMIES"
sco6		.byte $DC,$2E,$16,$48,$1E,$00,$1E,$30,$1E,$2E,$26,$1E,$BA
		
		;"POINTS FOR DESTROYING WITH SHIELDS"
sco6a		.byte $9A,$34,$32,$26,$30,$3C,$3A,$00,$20,$32,$38,$00,$1C,$1E,$3A,$3C
		.byte $38,$32,$46,$26,$30,$22,$00,$42,$26,$3C,$24,$00,$3A,$24,$26,$1E
		.byte $2C,$1C,$BA
		
		;"   PYROIDS         1000           "
sco7		.byte $9A,$00,$00,$00,$34,$46,$38,$32,$26,$1C,$3A,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$04,$02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$80
		
		;"  PERKOIDS         1000           "
sco8		.byte $9A,$00,$00,$34,$1E,$38,$2A,$32,$26,$1C,$3A,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$04,$02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$80
		
		;"                                  "
sco9		.byte $80,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C
		.byte $4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C
		.byte $4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$CC
		
		;"OTHER POINTS"
sco10		.byte $DC,$32,$3C,$24,$1E,$38,$00,$34,$32,$26,$30,$3C,$BA
		
		;"  REACTOID         5000           "
sco11		.byte $9A,$00,$00,$38,$1E,$16,$1A,$3C,$32,$26,$1C,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$0C,$02,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$80
		
		;"   OXOID           100           "
sco12		.byte $9A,$00,$00,$00,$00,$32,$44,$32,$26,$1C,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$04,$02,$02,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$80
		
		;"    OXOID        VARIES           "
sco13		.byte $9A,$00,$00,$00,$00,$32,$44,$32,$26,$1C,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$40,$16,$38,$26,$1E,$3A,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$80
		
		;"                                  "
sco13a	.byte $80,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C
		.byte $4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C
		.byte $4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$CC
		
		;"OTHER SCORES"
sco14		.byte $DC,$32,$3C,$24,$1E,$38,$00,$3A,$1A,$32,$38,$1E,$BA
		
		;"OXYGEN BONUS FOR EXITING MAZE IS 100 TIMES"
sco15		.byte $90,$32,$44,$46,$22,$1E,$30,$00,$18,$32,$30,$3E,$3A,$00,$20,$32
		.byte $38,$00,$1E,$44,$26,$3C,$26,$30,$22,$00,$2E,$16,$48,$1E,$00,$26
		.byte $3A,$00,$04,$02,$02,$00,$3C,$26,$2E,$1E,$BA
		
		;"THE NUMBER OF OXYGEN COUNTS LEFT"
sco16		.byte $90,$3C,$24,$1E,$00,$30,$3E,$2E,$18,$1E,$38,$00,$32,$20,$00,$32
		.byte $44,$46,$22,$1E,$30,$00,$1A,$32,$3E,$30,$3C,$3A,$00,$2C,$1E,$20
		.byte $3C,$CC
		
		;"IN SPACE, FASTER COMPLETION OF EACH WAVE"
sco17		.byte $90,$26,$30,$00,$3A,$34,$16,$1A,$1E,$52,$00,$20,$16,$3A,$3C,$1E
		.byte $38,$00,$1A,$32,$2E,$34,$2C,$1E,$3C,$26,$32,$30,$00,$32,$20,$00
		.byte $1E,$16,$1A,$24,$00,$42,$16,$40,$9E
		
		;"AWARDS MORE BONUS POINTS AT END OF WAVE"
sco18		.byte $90,$16,$42,$16,$38,$1C,$3A,$00,$2E,$32,$38,$1E,$00,$18,$32,$30
		.byte $3E,$3A,$00,$34,$32,$26,$30,$3C,$3A,$00,$16,$3C,$00,$1E,$30,$1C
		.byte $00,$32,$20,$00,$42,$16,$40,$1E,$CC
		
		;"                                         "
sco19		.byte $80,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C
		.byte $4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C
		.byte $4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$CC
		
		;"INSTRUCTIONS"
sco20		.byte $DC,$26,$30,$3A,$3C,$38,$3E,$1A,$3C,$26,$32,$30,$BA
		
		;"1: USE TACT SCAN FOR WARPING AND INFORMATION"
sco21		.byte $90,$04,$4C,$00,$3E,$3A,$1E,$00,$3C,$16,$1A,$3C,$00,$3A,$1A,$16
		.byte $30,$00,$20,$32,$38,$00,$42,$16,$38,$34,$26,$30,$22,$00,$16,$30
		.byte $1C,$00,$26,$30,$20,$32,$38,$2E,$16,$3C,$26,$32,$B0
		
		;"2: DESTROY MOTHERSHIP DEFENSES"
sco22		.byte $90,$06,$4C,$00,$1C,$1E,$3A,$3C,$38,$32,$46,$00,$2E,$32,$3C,$24
		.byte $1E,$38,$3A,$24,$26,$34,$00,$1C,$1E,$20,$1E,$30,$3A,$1E,$BA
		
		;"3: LAND ON WHITE PLATFORM OF DEFEATED MOTHERSHIP"
sco23		.byte $90,$08,$4C,$00,$2C,$16,$30,$1C,$00,$32,$30,$00,$42,$24,$26,$3C
		.byte $1E,$00,$34,$2C,$16,$3C,$20,$32,$38,$2E,$00,$32,$20,$00,$1C,$1E
		.byte $20,$1E,$16,$3C,$1E,$1C,$00,$2E,$32,$3C,$24,$1E,$38,$3A,$24,$26
		.byte $B4
		
		;"4: IN MAZE, FIND THE REACTOID AND TOUCH IT"
sco24		.byte $90,$0A,$4C,$00,$26,$30,$00,$2E,$16,$48,$1E,$52,$00,$20,$26,$30
		.byte $1C,$00,$3C,$24,$1E,$00,$38,$1E,$16,$1A,$3C,$32,$26,$1C,$00,$16
		.byte $30,$1C,$00,$3C,$32,$3E,$1A,$24,$00,$26,$BC
		
		;"5: EXIT MAZE BEFORE REACTIOD BLOWS UP"
sco25		.byte $90,$0C,$4C,$00,$1E,$44,$26,$3C,$00,$2E,$16,$48,$1E,$00,$18,$1E
		.byte $20,$32,$38,$1E,$00,$38,$1E,$16,$1A,$3C,$32,$26,$1C,$00,$18,$2C
		.byte $32,$42,$3A,$00,$3E,$B4
		
		;"  "
sco26		.byte $FA,$00,$80
		
		;"NOTE:: HOLD JUMP BUTTON FOR HIGHER JUMPS"
sco27		.byte $90,$30,$32,$3C,$1E,$50,$50,$24,$32,$2C,$1C,$00,$28,$3E,$2E,$34
		.byte $00,$18,$3E,$3C,$3C,$32,$30,$00,$20,$32,$38,$00,$24,$26,$22,$24
		.byte $1E,$38,$00,$28,$3E,$2E,$34,$BA
		
	
lngtb3	.word engmsg3

;**********************************************
	.sbttl "TWMSG2 - Message Tables #2"
;**********************************************
lstmsg2   = 0
___msnum2 = 0
___nmsgs2 = 27d
___eng2 = $
___csy2 = ___eng2+(___nmsgs2*2)
engmsg2 = ___eng2
msglbs2 = ___csy2

#define 	mess2(xlit2,xcol2,xsca2,xypo2)  \ .org ___eng2
#defcont   \lstmsg2 .set lstmsg2+2
#defcont   \ .word xlit2
#defcont   \___eng2 .set ___eng2+2
#defcont   \m+xlit2 = ___msnum2
#defcont   \___msnum2 .set ___msnum2+2
#defcont   \ .org ___csy2
#defcont   \ .byte ((xcol2*$10)|(xsca2))
#defcont   \ .byte xypo2
#defcont   \___csy2 .set ___csy2+2

	mess2(text0,colwhite,1,$10)		;Story
	mess2(text1,colwhite,1,$10)
	mess2(text2,colwhite,1,$10)
	mess2(text3,colwhite,1,$10)
	mess2(text4,colcyan,1,$10)
	mess2(text5,colcyan,1,$10)
	mess2(text6,colcyan,1,$10)
	mess2(text7,colcyan,1,$10)
	mess2(text8,colgreen,1,$10)
	mess2(text9,colgreen,1,$10)
	mess2(text10,colgreen,1,$10)
	mess2(text11,colgreen,1,$10)
	mess2(text12,colyellow,1,$10)
	mess2(text13,colyellow,1,$10)
	mess2(text14,colyellow,1,$10)
	mess2(text15,colyellow,1,$10)
	mess2(text16,colyellow,1,$10)
	mess2(text17,colyellow,1,$10)
	mess2(text18,colpurple,1,$10)
	mess2(text19,colpurple,1,$10)
	mess2(text20,colblue,1,$10)		;Story End
	mess2(txtc1,colpurple,1,$10)		;Story Cost
	mess2(txtc2,colpurple,1,$10)		;Story Cost
	mess2(txtc3,colpurple,1,$10)
	
lstmsg2 .set lstmsg2-6		;Drop these from "Story Line Count"

		;"   YOU ARE MAJOR HAVOC, THE LEADER OF A BRAVE"
text0		.byte $90,$00,$00,$00,$46,$32,$3E,$00,$16,$38,$1E,$00,$2E,$16,$28,$32
		.byte $38,$00,$24,$16,$40,$32,$1A,$52,$00,$3C,$24,$1E,$00,$2C,$1E,$16
		.byte $1C,$1E,$38,$00,$32,$20,$00,$16,$00,$18,$38,$16,$40,$9E
		
		;"LITTER OF CLONES. YOU ARE THEM, THEY ARE YOU,"
text1		.byte $90,$2C,$26,$3C,$3C,$1E,$38,$00,$32,$20,$00,$1A,$2C,$32,$30,$1E
		.byte $3A,$4C,$00,$46,$32,$3E,$00,$16,$38,$1E,$00,$3C,$24,$1E,$2E,$52
		.byte $00,$3C,$24,$1E,$46,$00,$16,$38,$1E,$00,$46,$32,$3E,$D2
		
		;"ALL FROM ONE, ONE FROM ALL, FIGHTING FOR"
text2		.byte $90,$16,$2C,$2C,$00,$20,$38,$32,$2E,$00,$32,$30,$1E,$52,$00,$32
		.byte $30,$1E,$00,$20,$38,$32,$2E,$00,$16,$2C,$2C,$52,$00,$20,$26,$22
		.byte $24,$3C,$26,$30,$22,$00,$20,$32,$B8
		
		;"HUMANITY     "
text3		.byte $90,$24,$3E,$2E,$16,$30,$26,$3C,$46,$4C,$4C,$4C,$4C,$CC
		
		;"   EONS AGO THE EVIL VAXXIAN EMPIRE OVERRAN"
text4		.byte $90,$00,$00,$00,$00,$1E,$32,$30,$3A,$00,$16,$22,$32,$00,$3C,$24
		.byte $1E,$00,$1E,$40,$26,$2C,$00,$40,$16,$44,$44,$26,$16,$30,$00,$1E
		.byte $2E,$34,$26,$38,$1E,$00,$32,$40,$1E,$38,$38,$16,$B0
		
		;"THE GALAXY. MOST OF YOUR ANCESTORS WERE"
text5		.byte $90,$00,$3C,$24,$1E,$00,$22,$16,$2C,$16,$44,$46,$4C,$00,$2E,$32
		.byte $3A,$3C,$00,$32,$20,$00,$46,$32,$3E,$38,$00,$16,$30,$1A,$1E,$3A
		.byte $3C,$32,$38,$3A,$00,$42,$1E,$38,$9E
		
		;"ENSLAVED AND TAKEN TO THE VAXXIAN HOMEWORLD"
text6		.byte $90,$1E,$30,$3A,$2C,$16,$40,$1E,$1C,$00,$16,$30,$1C,$00,$3C,$16
		.byte $2A,$1E,$30,$00,$3C,$32,$00,$3C,$24,$1E,$00,$40,$16,$44,$44,$26
		.byte $16,$30,$00,$24,$32,$2E,$1E,$42,$32,$38,$2C,$9C
		
		;"OF MAYNARD. ONLY A FEW SCIENTISTS ESCAPED."
text7		.byte $90,$32,$20,$00,$2E,$16,$46,$30,$16,$38,$1C,$4C,$00,$32,$30,$2C
		.byte $46,$00,$16,$00,$20,$1E,$42,$00,$3A,$1A,$26,$1E,$30,$3C,$26,$3A
		.byte $3C,$3A,$00,$1E,$3A,$1A,$16,$34,$1E,$1C,$CC
		
		;"   TODAY, THEIR EMPIRE IS ALL BUT VANISHED."
text8		.byte $90,$00,$00,$00,$3C,$32,$1C,$16,$46,$52,$00,$3C,$24,$1E,$26,$38
		.byte $00,$1E,$2E,$34,$26,$38,$1E,$00,$26,$3A,$00,$16,$2C,$2C,$00,$18
		.byte $3E,$3C,$00,$40,$16,$30,$26,$3A,$24,$1E,$1C,$CC
		
		;"YET VAXXIAN SPACE STATIONS, CONTROLLED AND"
text9		.byte $90,$46,$1E,$3C,$00,$40,$16,$44,$44,$26,$16,$30,$00,$3A,$34,$16
		.byte $1A,$1E,$00,$3A,$3C,$16,$3C,$26,$32,$30,$3A,$52,$00,$1A,$32,$30
		.byte $3C,$38,$32,$2C,$2C,$1E,$1C,$00,$16,$30,$9C
		
		;"DEFENDED BY ROBOTS, STILL PATROL THE GALAXY"
text10		.byte $90,$1C,$1E,$20,$1E,$30,$1C,$1E,$1C,$00,$18,$46,$00,$38,$32,$18
		.byte $32,$3C,$3A,$52,$00,$3A,$3C,$26,$2C,$2C,$00,$34,$16,$3C,$38,$32
		.byte $2C,$00,$3C,$24,$1E,$00,$22,$16,$2C,$16,$44,$C6
		
		;"AND KEEP YOUR PEOPLE PRISONER"
text11	.byte $90,$16,$30,$1C,$00,$2A,$1E,$1E,$34,$00,$46,$32,$3E,$38,$00,$34
		.byte $1E,$32,$34,$2C,$1E,$00,$34,$38,$26,$3A,$32,$30,$1E,$38,$CC
		
		;"   THE SMALL BAND SCIENTISTS CLONED YOU,"
text12	.byte $90,$00,$00,$00,$3C,$24,$1E,$00,$3A,$2E,$16,$2C,$2C,$00,$18,$16
		.byte $30,$1C,$00,$32,$20,$00,$3A,$1A,$26,$1E,$30,$3C,$26,$3A,$3C,$3A
		.byte $00,$1A,$2C,$32,$30,$1E,$1C,$00,$46,$32,$3E,$D2
		
		;"MAJOR HAVOC, TO FLY YOUR CATASTROFIGHTER"
text13	.byte $90,$2E,$16,$28,$32,$38,$00,$24,$16,$40,$32,$1A,$52,$00,$3C,$32
		.byte $00,$20,$2C,$46,$00,$46,$32,$3E,$38,$00,$1A,$16,$3C,$16,$3A,$3C
		.byte $38,$32,$20,$26,$22,$24,$3C,$1E,$B8
		
		;"THROUGH A WORMHOLE IN SPACE AND TO LEAD YOUR"
text14	.byte $90,$3C,$24,$38,$32,$3E,$22,$24,$00,$16,$00,$42,$32,$38,$2E,$24
		.byte $32,$2C,$1E,$00,$26,$30,$00,$3A,$34,$16,$1A,$1E,$00,$16,$30,$1C
		.byte $00,$3C,$32,$00,$2C,$1E,$16,$1C,$00,$46,$32,$3E,$B8
		
		;"CLONE ARMY AGAINST THE DREADED VAXXIAN ROBOT"
text15	.byte $90,$1A,$2C,$32,$30,$1E,$00,$16,$38,$2E,$46,$00,$16,$22,$16,$26
		.byte $30,$3A,$3C,$00,$3C,$24,$1E,$00,$1C,$38,$1E,$16,$1C,$1E,$1C,$00
		.byte $40,$16,$44,$44,$26,$16,$30,$00,$38,$32,$18,$32,$BC
		
		;"ARMADA. DESTROY THE ENEMY SPACE STATIONS AND"
text16	.byte $90,$16,$38,$2E,$16,$1C,$16,$4C,$00,$1C,$1E,$3A,$3C,$38,$32,$46
		.byte $00,$3C,$24,$1E,$00,$1E,$30,$1E,$2E,$46,$00,$3A,$34,$16,$1A,$1E
		.byte $00,$3A,$3C,$16,$3C,$26,$32,$30,$3A,$00,$16,$30,$9C
		
		;"LAND ON THE PLANET OF VAXXX TO FREE YOUR PEOPLE."
text17	.byte $90,$2C,$16,$30,$1C,$00,$32,$30,$00,$3C,$24,$1E,$00,$34,$2C,$16
		.byte $30,$1E,$3C,$00,$32,$20,$00,$40,$16,$44,$44,$44,$00,$3C,$32,$00
		.byte $20,$38,$1E,$1E,$00,$46,$32,$3E,$38,$00,$34,$1E,$32,$34,$2C,$1E
		.byte $CC
		
		;" DEPOSIT COINS NOW TO FINANCE YOUR CLONE ARMY."
text18	.byte $90,$00,$1C,$1E,$34,$32,$3A,$26,$3C,$00,$1A,$32,$26,$30,$3A,$00
		.byte $30,$32,$42,$00,$3C,$32,$00,$20,$26,$30,$16,$30,$1A,$1E,$00,$46
		.byte $32,$3E,$38,$00,$1A,$2C,$32,$30,$1E,$00,$16,$38,$2E,$46,$CC
		
		;"YOU MAY EVEN EARN A BONUS CLONE NOW AND THEN."
text19	.byte $90,$46,$32,$3E,$00,$2E,$16,$46,$00,$1E,$40,$1E,$30,$00,$1E,$16
		.byte $38,$30,$00,$16,$00,$18,$32,$30,$3E,$3A,$00,$1A,$2C,$32,$30,$1E
		.byte $00,$30,$32,$42,$00,$16,$30,$1C,$00,$3C,$24,$1E,$30,$CC
		
		;"      END OF TEXT     "
text20	.byte $C1,$50,$50,$50,$50,$50,$1E,$30,$1C,$00,$32,$20,$00,$3C,$1E,$44
		.byte $3C,$50,$50,$50,$50,$D0
		
		;"   CLONES ARE FREE, WE ARE GIVING THEM AWAY."
txtc1		.byte $90,$00,$00,$00,$1A,$2C,$32,$30,$1E,$3A,$00,$16,$38,$1E,$00,$20
		.byte $38,$1E,$1E,$52,$00,$42,$1E,$00,$16,$38,$1E,$00,$22,$26,$40,$26
		.byte $30,$22,$00,$3C,$24,$1E,$2E,$00,$16,$42,$16,$46,$CC
		
		;"  FOR ONLY 1 COIN YOU CAN NOW FINANCE 3 CLONES"
txtc2		.byte $90,$00,$00,$00,$20,$32,$38,$00,$32,$30,$2C,$46,$00,$04,$00,$1A
		.byte $32,$26,$30,$00,$46,$32,$3E,$00,$1A,$16,$30,$00,$30,$32,$42,$00
		.byte $20,$26,$30,$16,$30,$1A,$1E,$00,$08,$00,$1A,$2C,$32,$30,$1E,$3A
		.byte $CC
		
		;"   FOR ONLY 2 COINS YOU CAN NOW FINANCE 5 CLONES."
txtc3		.byte $90,$00,$00,$00,$20,$32,$38,$00,$32,$30,$2C,$46,$00,$06,$00,$1A
		.byte $32,$26,$30,$3A,$00,$46,$32,$3E,$00,$1A,$16,$30,$00,$30,$32,$42
		.byte $00,$20,$26,$30,$16,$30,$1A,$1E,$00,$0C,$00,$1A,$2C,$32,$30,$1E
		.byte $3A,$CC
		
lngtb2	.word engmsg2 
		
;***********************************************
	.sbttl "Message Output 2"
;***********************************************
;* Will output message to specified location   *
;* on screen.                                  *
;***********************************************
msg2		stx	temp4
		ldy	temp4				;Set up pointer to litera2
		lda	(litra2,Y)
		sta	temp5
		iny	
		lda	(litra2,Y)
		sta	temp5+1			;Set literal pointer
		ldy	#$00
		sty	vgbrit			;Position dark
		lda	(temp5,Y)			;Get H position 
		sta	xcomp				;X LSB/4 position
		lda	#$FF
		sta	xcomp+1
		asl	xcomp
		rol	xcomp+1			;Multiply by 4
		asl	xcomp
		rol	xcomp+1			;X position
		lda	#$02
		jsr	vgsca1			;Set position scale
		jsr	vgcntr
		ldx	#xcomp
		jsr	vgvctr			;Position
		ldx	temp4
		lda	msglbs2,X
		pha	
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		ora	#$F0
		tay	
		lda	#$00
		jsr	vgstat			;Set color
		pla	
		and	#$0F
		clc	
		adc	#$01
		ldy	#$5C
		jsr	vgscal			;Set scale
		ldy	#$01
		lda	#$00				;C init vglist offset
		sta	temp1
		begin
			lda	(temp5,Y)			;C Get character representation
			sta	temp2
			and	#$7F
			iny	
			sty	temp3				;Save Y
			tax	
			lda	vgmsga,X			;C Get correct JSRL
			bit	foreign			;D Want 'foreign' letters?
			ifmi
				lda	formsg,X			;C Get alternate char set
			endif
			ldy	temp1
			sta	(vglist,Y)
			iny	
			lda	vgmsga+1,X
			bit	foreign			;D Want 'foreign' letters?
			ifmi
				lda	formsg+1,X			;C Get alternate char set
			endif
			sta	(vglist,Y)
			iny	
			sty	temp1				;Save Y
			ldy	temp3				;C Get character ptr
			bit	temp2				;D If not end of string
		miend
		ldy	temp1				;C Update List
		dey	
		jmp	vgadd
	
;*******************************************
	.sbttl "Story Output"
;*******************************************
story		lda	strtst			;Story status on??
		ifpl
			rts
		endif
		lda	#$00
		sec	
		adc	strtyl
		tay					;Save LSB	
		lda	strtyh
		adc	#$00
		ifpl					;This line is gone
			inc	strtln
			inc	strtln			;By 2's
			tax					;Save this too	
			tya	
			sec	
			sbc	#$80				;Distance between lines
			tay
			txa
			sbc	#$00
		endif
		sta	strtyh
		sta	xcomp+3			;Will be Y MSB
		sty	strtyl
		sty	xcomp+2			;Is LSB
		ldx	#$03				;Will do 4 lines
		stx	temp9
		lda	strtln			;First line to output
		cmp	#lstmsg2			;Done all??
		ifcs
			lda	#$00				;Set to end of text
			sta	strtst			;Change status
			rts					;Return
		endif
		sta	temp8
		begin
			ldx	temp8
			cpx	#lstmsg2			;Past last message
			bcs	done2
			cpx	#$24				;Coin mode??
			ifeq
				ldy	slives			;Save this for check
				lda	_cmode			;Which coin mode??
				and	#$03
				ifeq
					ldx	#$2A				;Do free play message
				endif
				cmp	#$03				;2 coins 1 play
				ifeq
					cpy	#$05
					ifeq
						ldx	#$2E				;Do 2 coin message
					endif
				endif
				cmp	#$02				;1 Coin 1 Play
				ifeq
					cpy	#$03				;Should have 3 lives
					ifeq
						ldx	#$2C				;Do 1 Coin message
					endif
				endif
			endif
			;*** All others default to message 24, Insert Coin Message
			jsr	msg2				;Do this line
			inc	temp8
			inc	temp8				;To next line
			lda	xcomp+2			;Move down some space
			sec
			sbc	#$80
			sta	xcomp+2
			ifcc
				dec	xcomp+3			;To next line
			endif
			dec	temp9				;Any more lines?
		miend
done2 	rts   


	.nocodes		;So we dont have list file buffer overflows
	.fill $4000-*
	.end
	
.export msg3,stor3,story,lngtb2,lngtb3