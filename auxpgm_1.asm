 .locallabelchar "?"
#include "logic.ah"
#include "vector.ah"
#include "mh_vrom.exp"
#include "mh_alpha.exp"
 .module aux1
;********************************************
;* Major Havoc                              *
;********************************************
	.title "TWMAZE - Maze Routines"
	.sbttl "Globals"
	
	.org $2000
	
;nmrob		=	10d
;nmfire	=	16d
;nmlsht	=	8
;nmcann	=	4
;nmshot	=	nmrob
;nmdisc	=	16d
;nmligh	=	7
;nmfrfl	=	7
;nmtrpp	=	8
;nmonew	=	4
;nmarow	=	10d
;nmtite	=	5
;nmtran	=	8
;nmlock	=	4
;nmkeys	=	4
;nmstuf	=	4
numaze	=	16d		;Number of Mazes

; Mazes 17-20 can initialize robots, fireballs, laser cannons, oxygen discs, lightning,
; trip pads, one way walls, arrows, stalactites, transporters, locks and keys.
; Cannot initialize differently boots, clock, pod, reactor time.

cksum8	.byte $EB

;**************************************************
	.sbttl "Draw Maze"
;**************************************************
;* This routine will draw the maze on the screen  *
;* based on the values given in maze X and maze Y *
;**************************************************
drawm		jsr	opcl				;Open close doors
		lda	#00
		sta	linen				;Start with line 0
		lda	#$30
		ldx	#$72				;QQQSCALQQQ
		jsr	vgadd2
		lda	#mazcol+$e0
		bit	objst+zreactor
		ifmi					;Blowing up?
			ldy	retime			;Not yet
			cpy	#$10				;Only during last 10 counts
			ifcc					;yeah, do this
				lda	frame				;Flash color
				lsr	A
				ora	#$A0
			endif
		endif
		ldx	#maz7				;Stat page select
		ldy	tspark
		ifne
			ldx	#maz7+8			;Sparkle it
		endif
		bit	nodraw			;Skip draw?
		ifmi
			lda	#00				;Draw, but do it black
		endif
		jsr	vgadd2
		jsr	getptr			;Get input pointer
		lda	mazexh			;Do we need to change position?
		ifmi					;yes
			jsr	neg
		else
			lda	#00			;else use 0
		endif
		sec	
		sbc	#xoffset
		sta	xcomp+1
		lda	mazexl
		ifeq					;0 is a problem
			inc	mazexl
			lda	#01
		endif
		jsr	neg
		sta	xcomp
		lda	#ytop
		bit	mazeyh			;Need to move down?
		ifpl					;yep
			sec	
			sbc	mazeyh
		endif
		sta	xcomp+3
		clc	
		adc	#vunits-1			;Number of units to draw
		sta	temp4				;Units to draw
		lda	mazeyl
		ifeq
			inc	mazeyl
			lda	#01
		endif
		jsr	neg
		sta	xcomp+2
		clc	
		adc	ymot				;Add in scroll amount
		sta	xcomp+2
		lda	#00
		adc	#00				;Save the carry
		sta	temp8+1
		lda	#00
		sta	vgbrit
		lda	#06
		sta	temp3				;Initial Guess
		bit	mazeyh
		ifmi
			lda	mazeyh
			sec	
			sbc	ymot+1
			sbc	temp8+1			;Prop carry from above
			bit	ymot+1			;The famous fudge factor
			ifmi
				clc
				adc	#01
			endif
			jsr	neg
			tax					;Number of lines to skip
			begin
				jsr	skunit			;Skip H line, point to start of next
				ifmi
					rts
				endif
				dex
			eqend
		else
			sec	
			sbc	mazeyh			;Skip this many
			sta	temp3				;Temp3 hold line count
		endif
		lda	xcomp				;Add in camera scroll
		clc	
		adc	xmot				;From targx routine
		sta	xcomp
		lda	xcomp+1
		bit	xmot+1			;Which way??
		ifmi
			lda	#00				;For right hand scroll
			adc	#00
			sta	temp8
		else
			adc	xmot+1			;if moving right, draw different
			sta	xcomp+1
			ifpl					;Drawing Right???
				cmp	#01				;Skip if drawing too far off right
				ifcs
					rts
				endif
			endif
		endif
		begin
			ldy	#02
			lda	(mazpt,Y)			;Is this a blank line?
			ifne
				jsr	vgcntr			;Return to center for more
				jsr	vgvtr2
				jsr	unit				;Draw a line
			endif
			jsr	skunit			;else skip this one
			bmi	done
			dec	xcomp+3
			dec	temp4				;Another line?
		miend

done		lda	temp4
		ifpl					;Skip if minus entry
			ifne					;Need extra lines
				lda	#00
				sta	vgbrit
				begin
					lda	#-3
					sta	xcomp+1		;Long H line
					jsr	vgvtr2
					dec	temp4
				miend
			endif
		endif
		rts
		
;*************************************************
	.sbttl "Open/Close Doors"
;*************************************************
;* This routine will open and close the main     *
;* doors at the proper time.                     *
;*************************************************			
opcl		lda	objst+zreactor			;Reactor status
		ifmi						;Blowing up?
			lda	#$80
			sta	openflg				;Set flag to open
			lda	objst+zstuf+3
			ifne
				lda	daccy
				bne	?ocd10
			endif
			lda	maznum
			cmp	#01
			ifeq
				ldx	difcty
				lda	mpod,X				;Determine if doors should open
				cmp	#02
				beq	?ocd10				;Nope!
			endif
			jsr	ocdat					;Pointer to data in Y
			iny
			iny						;Pass two bytes close data
			bne	?bad1					;Output new bytes
		else
			lda	gamest
			and	#$20					;In Maze??
			ifne						;yep
				bit	mzgrnd				;On the ground??
				ifmi						;Okay to do this now
					lda	openflg				;Maze open??
					ifmi						;Yes, close it!
?ocd10					lda	#00
						sta	openflg				;Set open bit
						jsr	ocdat					;Pointer to data in Y
?bad1						lda	(mazpt,Y)
						sta	mazer,X				;Store new data
						iny
						inx
						lda	(mazpt,Y)
						sta	mazer,X
					endif
				endif
			endif
		endif
		rts
			
ocdat		lda	maznum
		asl	A
		tay	
		lda	mzocd+1,Y
		sta	mazpt+1
		lda	mzocd,Y
		sta	mazpt
		ldy	#00
		lda	(mazpt,Y)					;First Byte
		tax							;RAM output location pointer
		; (mazpt,Y) points to close data at this point
		iny
		rts
		
;**************************************************
	.sbttl "Trap Doors (Or shut your trap!)"
;**************************************************
;* For Trap Doors:                                *
;* Database:	Index to pg1,offset to time,    *
;*  			time, open data, close data     *
;*                                                *
;* Executed once every 16 frames minimum          *
;**************************************************
trapal	lda	dif4mz			;Executed only once, permanant changes
		asl	A
		tax	
		lda	mztdal,X
		sta	temp3
		lda	mztdal+1,X
		sta	temp3+1
		ldy	#00
?td10		lda	(temp3,Y)
		ifeq
?td20			rts
		endif
		tax	
		iny	
		lda	(temp3,Y)
		sta	mazer,X
		iny	
		jmp	?td10
		
trap		lda	frame
		and	#$0F
		eor	#$0F
		ifne
			rts
		endif
trap2		lda	dif4mz
		asl	A
		tax					;Get pointer for this maze
		cmp	#$10
		bcc	?trp20
		lda	mztd-$10,X			;Trap door data
		sta	temp3
		lda	mztd-$0f,X
		sta	temp3+1			;Ind pointer
		ldy	#00
		lda	frame+1
		sta	temp1
		lda	frame
		ldx	#03
		begin
			lsr	temp1
			ror	A
			dex
		miend
		sta	temp1				;Time counter
?trp10	lda	(temp3,Y)			;Get index (0 = end)
		ifeq
?trp20		rts
		endif
		tax					;Save pointer to pg1
		iny					;Point to time additive
		lda	temp1
		clc	
		adc	(temp3,Y)			;Add time
		iny	
		and	(temp3,Y)			;Duty cycle
		ifeq
			iny	
			lda	(temp3,Y)			;Open it (less than or equal to half the time)
			iny
		else
			iny
			iny
			lda	(temp3,Y)
		endif
		iny
		sta	mazer,X			;Do this one
		jmp	?trp10

;***************************************************
	.sbttl "Skip Unit"
;***************************************************
;* Will skip one horiz unit and update pointers    *
;* for next unit (if one).                         *
;*                                                 *
;* Can also be used to skip to end of current unit.*
;***************************************************
skunit	inc	linen				;Next line
		jsr	unitp				;Get next pointers
		ldy	#00
		lda	(mazpt,Y)
		rts					;End of maze
		
;***************************************************
	.sbttl "Draw Unit (H line)"
;***************************************************
;* Draws 1 horizontal line of maze. Assumes        *
;* position of vector already set. Will draw 10d   *
;* segments or until end of current line, which    *
;* ever occurs first.                              *
;*                                                 *
;* RAM:	mazpt,mazpt+1	Pointer to current   *
;*                            maze stamp. 0 is end *
;*                            of line or end of    *
;*                            maze if the only     *
;*                            number in a unit.    *
;*                                                 *
;* Uses:	temp9, temp2                           *
;***************************************************
unit		ldy	#00				;For indirect pointers
		lda	#hunits-1			;10 stamps to right max
		sta	temp9				;Guess correct
		lda	mazexh
		ifmi					;Another weird fudge factor
			clc
			adc	#01
		endif
		bit	xmot+1			;Moving left or right?
		ifmi
			sec	
			sbc	xmot+1
			sbc	temp8
			clc	
			adc	#01
		endif
		tax
		ifne					;Skip for 0
			ifpl					;Skip for minus
				begin
					lda	(mazpt,Y)
					bpl	?dmu10			;End of maze if -
?dmu5					rts
?dmu10				beq	?dmu5				;End of unit if 0
					jsr	incmaz
					dex					;Any more stamps?
				eqend
			else						;We want to skip a few on right now
				lda	temp9
				clc						;A = temp9
				adc	mazexh				;Adding a negative number
				bit	xmot+1				;Did we move right?
				ifpl
					sec
					sbc	xmot+1				;Skip this many
				endif
				sta	temp9					;Update count (less than 12d)
			endif
		endif
		lda	(mazpt,Y)			;Get first stamp
		ifeq
			rts					;Started at end of line
		endif
		ifpl					;Not end of maze
			begin
				and	#$0F				;Stamp code
				asl	A				;Need a word pointer
				tay	
				lda	mazet-2,Y			;Get maze JSRL
				ldx	mazet-2+1,Y			;-2 because there is no stamp 0!!!!!
				jsr	vgadd2
				dec	temp9				;Did one
				bmi	badhab			;Escape out
				jsr	incmaz			;Bump pointer
				lda	(mazpt,Y)			;Next stamp
			eqend					;*** Always!!! ***
		else					;Must be end of maze
			rts					;Return A = 80
		endif			
badhab	rts

;***********************************************
	.sbttl "Maze 0 Data"
;***********************************************

maz0		
m0ua		.byte $47,$47,$00
m0ub		.byte $47,$47,$00
m0uc		.byte $47,$47,$00
m0ud		.byte $47,$47,$00
m0ue		.byte $47,$47,$00
m0uf		.byte $47,$47,$00

xxx0	=	1

m0u1		.byte $47,$47,$47,$47
		.byte $47,$47,$42,$45,$41,$41,$41,$41
		.byte $42,$47,$47,$00
		
xxx0 	.set  xxx0+1

m0u2		.byte $47,$47,$47,$47
		.byte $47,$45,$43,$04,$07,$01,$01,$02
		.byte $44,$42,$47,$00
		
xxx0 	.set  xxx0+1

m0u3		.byte $47,$47,$47,$47
		.byte $47,$46,$05,$01,$01,$07,$02,$06
		.byte $07,$46,$47,$00
		
xxx0 	.set xxx0+1

m0u4		.byte $47,$47,$47,$47
		.byte $45,$43,$06,$07,$01,$01,$03,$07
		.byte $06,$44,$42,$00
		
xxx0 	.set xxx0+1

m0u5		.byte $47,$47,$47,$47
		.byte $04,$02,$04,$01,$02,$07,$01,$01
		.byte $03,$05,$03,$00
		
xxx0 	.set xxx0+1

m0u6		.byte $47,$47,$47,$47
		.byte $47,$04,$01,$02,$04,$01,$07,$05
		.byte $01,$03,$47,$00
		
xxx0 	.set xxx0+1

mou7		.byte $47,$47,$47,$47
		.byte $47,$47,$47,$04,$01,$01,$01,$03
		.byte $47,$47,$07,$00
		
m0u8		.byte $80

m0cld		.byte $28		;Offset into RAM change place
		.byte $41,$03	;Closed Data
		.byte $43,$04	;Open Data
		
;***********************************************
	.sbttl "Maze 1 Data"
;***********************************************
maz1
m1ua		.byte $47,$47,$00
m1ub		.byte $47,$47,$00
m1uc		.byte $47,$47,$00
m1ud		.byte $47,$47,$00
m1ue		.byte $47,$47,$00
m1uf		.byte $47,$47,$00

xxx1	=	1

m1u1		.byte $47,$47,$47,$47
		.byte $47,$47,$47,$45,$42,$45,$41,$41
		.byte $41,$41,$41,$42,$47,$47,$47,$47
		.byte $00
		
xxx1	.set	xxx1+1

m1u2		.byte $47,$47,$47,$47
		.byte $47,$47,$45,$43,$46,$46,$05,$01
		.byte $01,$01,$07,$44,$41,$42,$47,$47
		.byte $00
		
xxx1	.set	xxx1+1		
		
m1u3		.byte $47,$47,$47,$47
		.byte $45,$41,$43,$01,$46,$46,$06,$05
		.byte $07,$01,$01,$01,$02,$44,$42,$45
		.byte $00
		
xxx1	.set	xxx1+1

m1u4		.byte $47,$47,$47,$47
		.byte $46,$07,$01,$02,$46,$46,$06,$04
		.byte $01,$01,$01,$02,$04,$02,$06,$46
		.byte $00
		
xxx1	.set	xxx1+1

m1u5		.byte $47,$47,$47,$47
		.byte $46,$05,$02,$06,$46,$46,$04,$01
		.byte $01,$01,$07,$06,$05,$03,$06,$46
		.byte $00
		
xxx1	.set	xxx1+1

m1u6		.byte $47,$47,$47,$47
		.byte $46,$04,$06,$04,$01,$01,$01,$01
		.byte $01,$01,$01,$03,$03,$05,$03,$46
		.byte $00
		
xxx1	.set	xxx1+1

m1u7		.byte $47,$47,$47,$47
		.byte $04,$02,$04,$07,$05,$01,$01,$01
		.byte $01,$01,$01,$02,$05,$03,$05,$03
		.byte $00
		
xxx1	.set	xxx1+1

m1u8		.byte $47,$47,$47,$47
		.byte $47,$46,$05,$01,$03,$07,$07,$07
		.byte $07,$07,$07,$04,$01,$01,$03,$47
		.byte $00
		
m1u9		.byte $80

m1cld		.byte $6E		;RAM offset Location
		.byte $44,$43	;Closed data
		.byte $46,$46	;Open data
		
;***********************************************
	.sbttl "Maze 2 Data"
;***********************************************
maz2
m2ua		.byte $47,$47,$00
m2ub		.byte $47,$47,$00
m2uc		.byte $47,$47,$00
m2ud		.byte $47,$47,$00
m2ue		.byte $47,$47,$00
m2uf		.byte $47,$47,$00

xxx2	=	1

m2u1		.byte $47,$47,$47,$47
		.byte $47,$47,$47,$47,$47,$42,$45,$41
		.byte $41,$41,$42,$47,$47,$47,$47,$47
		.byte $00
		
xxx2	.set 	xxx2+1	

m2u2		.byte $47,$47,$47,$47
		.byte $47,$47,$47,$45,$41,$43,$04,$07
		.byte $01,$02,$44,$41,$42,$47,$47,$47
		.byte $00
		
xxx2	.set 	xxx2+1

m2u3		.byte $47,$47,$47,$47
		.byte $45,$41,$41,$43,$05,$01,$01,$01
		.byte $02,$04,$01,$07,$44,$41,$42,$47
		.byte $00
		
xxx2	.set 	xxx2+1

m2u4		.byte $47,$47,$47,$47
		.byte $46,$07,$05,$01,$03,$05,$01,$07
		.byte $04,$01,$01,$01,$01,$07,$44,$42
		.byte $00
		
xxx2	.set 	xxx2+1

m2u5		.byte $47,$47,$47,$47
		.byte $46,$06,$04,$07,$01,$03,$05,$01
		.byte $01,$01,$02,$05,$01,$01,$02,$46
		.byte $00
		
xxx2	.set 	xxx2+1

m2u6		.byte $47,$47,$47,$47
		.byte $46,$04,$01,$01,$01,$02,$04,$07
		.byte $01,$01,$03,$05,$01,$01,$03,$46
		.byte $00
		
xxx2	.set 	xxx2+1

m2u7		.byte $47,$47,$47,$47
		.byte $04,$01,$01,$02,$07,$03,$05,$01
		.byte $07,$05,$01,$03,$05,$01,$01,$03
		.byte $00
		
xxx2	.set 	xxx2+1

m2u8		.byte $47,$47,$47,$47
		.byte $47,$47,$47,$04,$01,$02,$06,$07
		.byte $01,$03,$05,$02,$06,$47,$47,$47
		.byte $00
		
xxx2	.set 	xxx2+1

m2u9		.byte $47,$47,$47,$47
		.byte $47,$47,$47,$47,$47,$06,$04,$01
		.byte $01,$01,$03,$47,$47,$47,$47,$47
		.byte $00
		
m2u10		.byte $80

m2cld		.byte $30		;Offset Location
		.byte $41,$03	;Closed data
		.byte $43,$04	;Open data
		
;***********************************************
	.sbttl "Maze 3 Data"
;***********************************************		
maz3
m3ua		.byte $47,$47,$00
m3ub		.byte $47,$47,$00
m3uc		.byte $47,$47,$00
m3ud		.byte $47,$47,$00
m3ue		.byte $47,$47,$00
m3uf		.byte $47,$47,$00

xxx3	=	1

m3u1		.byte $47,$47,$47,$47
		.byte $47,$47,$47,$47,$42,$45,$41,$41
		.byte $41,$42,$47,$47,$47,$47,$00
		
xxx3	.set	xxx3+1

m3u2		.byte $47,$47,$47,$47
		.byte $47,$47,$45,$41,$43,$04,$07,$01
		.byte $02,$44,$41,$42,$47,$47,$00
		
xxx3	.set	xxx3+1

m3u3		.byte $47,$47,$47,$47
		.byte $45,$41,$43,$05,$01,$01,$01,$02
		.byte $04,$01,$02,$44,$41,$42,$00
		
xxx3	.set	xxx3+1

m3u4		.byte $47,$47,$47,$47
		.byte $04,$02,$07,$03,$05,$01,$07,$04
		.byte $01,$02,$04,$02,$05,$03,$00
		
xxx3	.set	xxx3+1

m3u5		.byte $47,$47,$47,$47
		.byte $47,$46,$05,$01,$03,$05,$01,$01
		.byte $07,$04,$07,$06,$46,$47,$00
		
xxx3	.set	xxx3+1

m3u6		.byte $47,$47,$47,$47
		.byte $47,$46,$06,$05,$01,$03,$07,$01
		.byte $01,$01,$01,$03,$46,$47,$00
		
xxx3	.set	xxx3+1

m3u7		.byte $47,$47,$47,$47
		.byte $47,$46,$06,$06,$07,$01,$01,$01
		.byte $01,$01,$02,$07,$46,$47,$00
	
xxx3	.set	xxx3+1

m3u8		.byte $47,$47,$47,$47
		.byte $45,$43,$06,$04,$01,$07,$05,$01
		.byte $01,$02,$04,$02,$44,$42,$00
		
xxx3	.set	xxx3+1

m3u9		.byte $47,$47,$47,$47
		.byte $04,$02,$04,$01,$01,$07,$04,$01
		.byte $01,$01,$01,$03,$05,$03,$00
		
xxx3	.set	xxx3+1

m3u10		.byte $47,$47,$47,$47
		.byte $47,$04,$01,$01,$01,$01,$01,$01
		.byte $01,$02,$05,$01,$03,$47,$00
		
xxx3	.set	xxx3+1

m3u11		.byte $47,$47,$47,$47
		.byte $47,$47,$47,$04,$01,$01,$01,$01
		.byte $01,$01,$03,$47,$47,$47,$00
		
m3u12		.byte $80

m3cld		.byte $2D		;RAM Offset
		.byte $41,$03	;Closed data
		.byte $43,$04	;Open data
		
;**************************************************
	.sbttl "Open/Closed Data Locations"
;**************************************************

mzocd		.word	m0cld,m1cld,m2cld,m3cld

;**************************************************
	.sbttl "Bit patterns for each stamp"
;**************************************************
;* These patterns are used to draw the little map *
;* at the top of the screen. For each stamp there *
;* is a byte which represents a bit rep of that   *
;* stamp. For example.. Stamp 1 -- H line will be *
;* $c0 (only top two bits are used) for the horiz *
;* rep and will be 0 for vert.                    *
;**************************************************

mapbits	.byte $00,$C0,$80,$80,$40,$40,$00,$00

mapvbt	.byte $00,$00,$40,$80,$80,$40,$C0,$00

;*** Maze Index Tables ***
;* Source Data Pointers  *

mazsrc	.word	maz0,maz1,maz2,maz3

;* Vertical Length Counters             *
;* Number of 'drawn' lines in each maze *

mazdep	.byte xxx0,xxx1,xxx2,xxx3

;* Horizontal Length of each maze             *
;* Assumption: Mazes are rectangular in shape *

mazlen	.byte m0u2-m0u1,m1u2-m1u1,m2u2-m2u1,m3u2-m3u1

;***************************************************
	.sbttl "Maze Init Data"
;***************************************************
;* Init Source Pointers *
mzinit	.word	mzsc00,mzsc10,mzsc20,mzsc30	;Difficulty=0
		.word	mzsc01,mzsc11,mzsc21,mzsc31	;Difficulty=1
		.word	mzsc02,mzsc12,mzsc22,mzsc32	;Difficulty=2
		.word	mzsc03,mzsc13,mzsc23,mzsc33	;Difficulty=3
		.word	mzsc03,mzsc13,mzsc23,mzsc33	;Difficulty=4
		
;* Init Source Disc Data *
mzdc		.word	mzdc0,mzdc1,mzdc2,mzdc3
		.word	mzdc0,mzdc1,mzdc2,mzdc3
		.word	mzdc0,mzdc1,mzdc2,mzdc3
		.word	mzdc30,mzdc31,mzdc2,mzdc3
		.word	mzdc30,mzdc31,mzdc2,mzdc3
		
;* Init Source Lightning Data *
mzlg		.word	mzlg00,mzlg01,mzlg02,mzlg03
		.word	mzlg10,mzlg11,mzlg12,mzlg13
		.word	mzlg20,mzlg21,mzlg22,mzlg23
		.word	mzlg30,mzlg31,mzlg32,mzlg33
		.word	mzlg30,mzlg31,mzlg32,mzlg33

;* Init Source Arrow Data *
mzar		.word mzar00,mzar01,mzar02,mzar03
		.word mzar10,mzar11,mzar12,mzar13
		.word mzar20,mzar21,mzar22,mzar23
		.word mzar30,mzar31,mzar32,mzar33
		.word mzar30,mzar31,mzar32,mzar33
		
;* Init Source Exit Arrow Data *
mzor		.word mzor00,mzor01,mzor02

;* Init Source Trip Point Data *
mztr		.word mztr10,mztr11,mztr12,mztr13
		.word mztr20,mztr21,mztr22,mztr23
		.word mztr30,mztr31,mztr32,mztr33
		.word mztr30,mztr31,mztr32,mztr33

;* Init Source Maze Adjustments *
mztdal	.word mzta00,mzta01,mzta02,mzta03
		.word mzta10,mzta11,mzta12,mzta13
		.word mzta20,mzta21,mzta22,mzta23
		.word mzta30,mzta31,mzta32,mzta33

;* Init Source Dynamic Maze Adjustments *
mztd		.word mztd20,mztd21,mztd22,mztd23
		.word mztd30,mztd31,mztd32,mztd33
		
;* Init Source One Way Walls *
mone		.word mone00,mone01,mone02,mone03
		.word mone10,mone11,mone12,mone13
		.word mone20,mone21,mone22,mone23
		.word mone30,mone31,mone32,mone33
		.word mone30,mone31,mone32,mone33
		
;* Init Source Ion Cannon's *
mcan		.byte 0,mcan01-mcanst,0,0
		.byte 0,0,0,0
		.byte 0,0,0,0
		.byte mcan30-mcanst,mcan31-mcanst,mcan32-mcanst,mcan33-mcanst
		.byte mcan40-mcanst,mcan31-mcanst,mcan32-mcanst,mcan33-mcanst		
		
;* Init Source Stalactites *
mtite		.word        tite11,tite12,tite13
		.word tite20,tite21,tite22,tite23
		.word tite30,tite31,tite32,tite33
		.word tite30,tite41,tite32,tite33
		
;* Init Source Locks *
mlock		.word lock00,lock00,lock00,lock03		
		.word lock00,lock00,lock00,lock00
		.word lock00,lock00,lock00,lock00
		.word lock30,lock00,lock32,lock33
		.word lock40,lock41,lock32,lock33

;* Init Source Transporter *
mtran		.word tran00,tran00,tran02,tran00	
		.word tran00,tran00,tran00,tran00
		.word tran00,tran00,tran00,tran00
		.word tran00,tran31,tran32,tran33
		.word tran00,tran31,tran32,tran33
		
;* Init Source De Hand *
mhand		.word               hand12,hand00
		.word hand00,hand00,hand00,hand00
		.word hand30,hand00,hand00,hand33
		.word hand30,hand00,hand00,hand33	
		
;**************************************************
	.sbttl "Reactor, Fireball, Robot Init"
;**************************************************
;* Init data in the following order:              *
;*                                                *
;* 	Reactor 	XL,XH,YL,YH                     *
;*	Fireballs	XL,XH,YL,YH,VELXH,VELYH		  *
;*    #FE signals end of fireball data            *
;*	#FF signals end of all data			  *
;**************************************************
;* Maze 0 -- Level 0
mzsc00	.byte $30,$0B,$40,$F8			;Reactor
		.byte $90,$03,$48,$F7,$10,$00 	;F0
		.byte $D8,$02,$48,$F7,$00,$10 	;F1
		.byte $D8,$02,$48,$F8,$00,$10		;F2
		.byte $01,$07,$48,$F9,$08,$00		;F3
		.byte $70,$05,$48,$F8,$82,$07,$00	;F4
		.byte $70,$06,$48,$F8,$82,$07,$00	;F5
		.byte $F0,$07,$48,$F8,$00,$10		;F6
		.byte $F0,$07,$48,$F9,$00,$10		;F7
		.byte $A0,$07,$48,$F7,$08,$00		;F8
		.byte $A0,$08,$48,$F7,$08,$00		;F9
		.byte $50,$06,$48,$F6,$10,$00		;FA
		.byte $FF
		
;* Maze 0 --Level 1
mzsc01	.byte $30,$0B,$40,$F8			;Reactor
		.byte $49,$09,$48,$F8,$00,$00		;F1
		.byte $FF
		
;* Maze 0 -- Level 2
mzsc02	.byte $30,$08,$40,$FB			;Reactor 
		.byte $49,$08,$48,$FA,$00,$00		;F1
		.byte $01,$07,$55,$F7,$00,$00		;F2
		.byte $01,$07,$AA,$F7,$00,$00		;F3
		.byte $01,$07,$48,$F7,$83,$0D,$00	;F4
		.byte $40,$07,$B8,$FA,$00,$00		;F4
		.byte $01,$08,$80,$FB,$00,$81,$07	;F5
		.byte $FE
		.byte $01,$03,$48,$F7,$83,$0D,$00	;R0
		.byte $01,$05,$48,$F8,$83,$0D,$00	;R1
		.byte $FF
		
;* Maze 0 -- Level 3
mzsc03	.byte $30,$07,$40,$F9			;Reactor
		.byte $80,$08,$80,$FB,$00,$81,$03	;F1
		.byte $00,$05,$48,$F6,$81,$07,$00	;F2
		.byte $00,$06,$48,$F6,$81,$07,$00	;F3
		.byte $C0,$04,$80,$F8,$00,$10		;F4
		.byte $C0,$04,$80,$F9,$00,$10		;F5
		.byte $FE
		.byte $00,$05,$48,$F8,$06,$00		;R0
		.byte $FF
		
;* Maze 1 -- Level 0
mzsc10	.byte $18,$0E,$49,$F8			;Reactor
		.byte $30,$08,$48,$F8,$00,$84,$0C	;F1
		.byte $30,$08,$48,$F9,$00,$84,$0C	;F2
		.byte $19,$09,$48,$F9,$82,$0E,$00	;F3
		.byte $38,$0B,$4A,$F7,$00,$7D,$F3	;F4
		.byte $FE
		.byte $40,$04,$01,$F7,$02,$00		;R0
		.byte $C0,$03,$01,$F8,$02,$00		;R1
		.byte $80,$0E,$4A,$F9,$04,$00		;R2
		.byte $9B,$0C,$49,$FA,$04,$00		;R3
		.byte $C0,$0D,$4A,$F7,$04,$00		;R4
		.byte $A3,$04,$00,$F8,$02,$00		;R5
		.byte $EE,$02,$4A,$F7,$00,$00		;R6
		.byte $A6,$06,$49,$F8,$81,$08,$00	;R7
		.byte $01,$07,$49,$F8,$81,$08,$00	;R8
		.byte $49,$0F,$C0,$F7,$08,$00		;R9
		.byte $FF
		
;* Maze 1 -- Level 1
mzsc11	.byte $18,$0E,$49,$F8			;Reactor
		.byte $80,$0A,$A0,$F9,$00,$00		;F0
		.byte $01,$08,$30,$F8,$00,$83,$0D	;F1
		.byte $18,$0B,$4A,$F7,$00,$7D,$F3	;F2
		.byte $A3,$04,$00,$F8,$18,$00		;F3
		.byte $70,$0C,$80,$F9,$00,$81,$09	;F4
		.byte $01,$0E,$48,$F5,$08,$00		;F5
		.byte $01,$0F,$48,$F5,$08,$00		;F6
		.byte $FE
		.byte $01,$08,$48,$F8,$83,$0D,$00	;R0
		.byte $EE,$02,$4A,$F7,$00,$00		;R1
		.byte $50,$04,$48,$F9,$10,$00		;R2
		.byte $60,$07,$00,$F9,$10,$00		;R3
		.byte $01,$07,$00,$F9,$10,$00		;R4
		.byte $FF
		
;* Maze 1 -- Level 2
mzsc12	.byte $18,$0E,$49,$F8			;Reactor
		.byte $40,$09,$A0,$F7,$00,$00		;F0
		.byte $60,$0C,$80,$F9,$00,$81,$09	;F1
		.byte $01,$0D,$40,$F8,$82,$0A,$00	;F3
		.byte $FE
		.byte $01,$08,$00,$F9,$08,$00		;R0
		.byte $01,$08,$00,$FA,$08,$00		;R1
		.byte $FF
		
;* Maze 1 -- Level 3
mzsc13	.byte $30,$0C,$40,$F7			;Reactor
		.byte $00,$04,$48,$FA,$10,$00		;F0
		.byte $00,$04,$00,$F8,$06,$00		;F1
		.byte $00,$0A,$80,$F7,$00,$81,$0B	;F3
		.byte $00,$0B,$80,$F7,$00,$81,$0B	;F4
		.byte $FE
		.byte $00,$04,$48,$F9,$83,$14,$00	;R0
		.byte $00,$02,$48,$F6,$81,$03,$00	;R1
		.byte $00,$0F,$80,$F5,$FF,$02		;R2
		.byte $80,$09,$80,$FB,$00,$02		;R3
		.byte $00,$09,$48,$F9,$00,$00		;R4
		.byte $FF
		
;* Maze 2 -- Level 0
mzsc20	.byte $20,$0B,$48,$F7			;Reactor
		.byte $37,$0C,$00,$FA,$00,$7C,$F4	;F0
		.byte $B8,$0C,$00,$FA,$00,$84,$0C	;F1
		.byte $FD,$0A,$00,$F5,$00,$10		;F2
		.byte $78,$08,$48,$FB,$00,$10		;F3
		.byte $FE
		.byte $01,$0A,$48,$F8,$84,$10,$00	;R0
		.byte $01,$0A,$B7,$F8,$84,$10,$00	;R1
		.byte $FD,$09,$48,$F5,$86,$0F,$00	;R2
		.byte $01,$04,$4A,$F7,$10,$00		;R3
		.byte $01,$0B,$4A,$F6,$FF,$00		;R4
		.byte $01,$0B,$B6,$F6,$FF,$00		;R5
		.byte $98,$06,$4A,$F9,$00,$00		;R6
		.byte $01,$04,$48,$F8,$10,$00		;R7
		.byte $FF
		
;* Maze 2 -- Level 1
mzsc21	.byte $20,$0B,$48,$F7			;Reactor
		.byte $90,$03,$80,$F9,$00,$81,$0B	;F0
		.byte $28,$04,$80,$F7,$00,$10		;F1
		.byte $D8,$04,$80,$F7,$00,$10		;F2
		.byte $01,$0A,$7C,$F9,$10,$00		;F3
		.byte $01,$0F,$7C,$F9,$10,$00		;F4
		.byte $FE
		.byte $80,$08,$48,$FA,$82,$0E,$00	;R0
		.byte $C0,$08,$80,$F7,$00,$81,$07	;R1
		.byte $80,$0B,$80,$F6,$00,$81,$07	;R2
		.byte $FF
		
;* Maze 2 -- Level 2
mzsc22	.byte $20,$0A,$48,$FA			;Reactor
		.byte $40,$0A,$01,$F8,$00,$00		;F0
		.byte $40,$10,$40,$F6,$00,$00		;F1
		.byte $E0,$01,$01,$F8,$02,$00		;F2
		.byte $E0,$01,$01,$F8,$FE,$00		;F3
		.byte $01,$0D,$80,$F8,$00,$81,$0C	;F4
		.byte $01,$0E,$80,$F8,$00,$81,$0C	;F5
		.byte $F8,$0E,$80,$F8,$00,$81,$0C	;F6
		.byte $00,$09,$80,$FB,$00,$81,$03	;F7
		.byte $FE
		.byte $01,$08,$14,$F7,$08,$00		;R0
		.byte $40,$09,$48,$FB,$FF,$00		;R1
		.byte $FF
		
;Maze 2 -- Level 3
mzsc23	.byte $20,$10,$48,$F6			;Reactor
		.byte $00,$0A,$48,$F9,$82,$16,$00	;F0
		.byte $80,$0C,$48,$F9,$82,$16,$00	;F1
		.byte $80,$09,$80,$F5,$81,$0A,$00	;F2
		.byte $00,$0C,$A0,$F4,$81,$05,$00	;F3
		.byte $00,$0C,$20,$F4,$81,$03,$00	;F4
		.byte $A0,$08,$C0,$F5,$81,$05,$00	;F5
		.byte $20,$0A,$40,$F5,$81,$05,$00	;F6
		.byte $FE
		.byte $20,$0B,$80,$F4,$FF,$00		;R0
		.byte $20,$0B,$00,$F4,$FF,$00		;R1
		.byte $20,$04,$48,$F6,$FF,$00		;R2
		.byte $80,$02,$00,$F9,$00,$81,$05	;R3
		.byte $FF
		
;* Maze 3 -- Level 0
mzsc30	.byte $28,$0C,$61,$F4			;Reactor
		.byte $60,$07,$48,$FA,$00,$84,$0C	;F0
		.byte $C0,$07,$48,$FA,$00,$84,$0C	;F1
		.byte $80,$0A,$48,$FA,$82,$0F,$00	;F2
		.byte $01,$0D,$48,$F8,$81,$07,$00	;F3
		.byte $01,$0D,$08,$F7,$81,$07,$00	;F4
		.byte $80,$06,$48,$F3,$10,$00		;F5
		.byte $80,$08,$48,$F3,$10,$00		;F6
		.byte $48,$0B,$48,$F4,$00,$20		;F7
		.byte $80,$03,$B0,$F8,$84,$14,$00	;F8
		.byte $FE
		.byte $01,$02,$48,$F9,$83,$0D,$00	;R0
		.byte $01,$07,$48,$F8,$10,$00		;R1
		.byte $01,$09,$48,$F7,$10,$00		;R2
		.byte $01,$04,$48,$F4,$10,$00		;R3
		.byte $FF
		
;* Maze 3 -- Level 1
mzsc31	.byte $28,$0E,$61,$F4			;Reactor
		.byte $01,$08,$80,$F6,$00,$81,$0B	;F0
		.byte $01,$09,$80,$F6,$00,$81,$0B	;F1
		.byte $01,$0A,$80,$F6,$00,$81,$0B	;F2
		.byte $80,$0A,$80,$F3,$00,$0E		;F3
		.byte $01,$07,$48,$F3,$10,$00		;F4
		.byte $FE
		.byte $01,$07,$48,$F7,$82,$16,$00	;R0
		.byte $01,$0A,$48,$F7,$82,$16,$00	;R1
		.byte $01,$04,$48,$F4,$18,$00		;R2
		.byte $40,$04,$40,$F5,$10,$00		;R3
		.byte $C0,$03,$40,$F6,$10,$00		;R4
		.byte $01,$08,$48,$F2,$0C,$00		;R5
		.byte $FF
		
;* Maze 3 -- Level 2
mzsc32	.byte $28,$0C,$61,$F4			;Reactor
		.byte $01,$0A,$80,$F6,$00,$82,$0C	;F0
		.byte $01,$05,$A0,$F6,$00,$00		;F1
		.byte $01,$07,$48,$F5,$18,$00		;F2
		.byte $01,$09,$48,$F5,$18,$00		;F3
		.byte $01,$08,$48,$F5,$E8,$00		;F4
		.byte $D0,$09,$48,$F4,$00,$00		;F5
		.byte $80,$0B,$48,$F6,$00,$00		;F6
		.byte $80,$0B,$B8,$F6,$00,$00		;F7
		.byte $FF
		
;* Maze 3 -- Level 3
mzsc33	.byte $30,$06,$61,$F3			;Reactor
		.byte $00,$0B,$48,$FA,$82,$10,$00	;F0
		.byte $00,$0D,$48,$F9,$82,$10,$00	;F1
		.byte $30,$08,$80,$F4,$00,$10		;F2
		.byte $D0,$08,$80,$F5,$00,$10		;F3
		.byte $30,$0A,$80,$F4,$00,$F0		;F4
		.byte $D0,$0A,$80,$F5,$00,$F0		;F5
		.byte $FF

;**************************************
	.sbttl "Disc Placement"    
;**************************************
;* -Y/XPosition Compressed Byte        *
;* Value of 0 ends list               *
;**************************************
mzdc0		.byte $A3,$B3
mzdc30		.byte $95,$83,$74,$77,$B6,$80,$00
mzdc1		.byte $53
mzdc31		.byte $60,$75,$93,$71,$7D,$7B,$7A,$97,$B6,$00
mzdc2		.byte $A6,$93,$71,$82,$88,$6A,$78,$56,$66,$5A,$6E,$8D,$9C,$00
mzdc3		.byte $A6,$99,$89,$77,$65,$57,$48,$3B,$40,$83,$24,$25,$26,$27,$28,$29 ;End is on next line
mzlg00		.byte $00

;**************************************
	.sbttl "Force Field Placement"
;**************************************
;* X + Direction, Y                   *
;* Note: First 7 horizontal, second   *
;*       7 vertical. FF finishes off  *
;*       Horizontal ones, 00 finishes *
;*       off list.                    *
;**************************************
;* Difcty Level 0
mzlg01	.byte $42,$9F,$7C,$00
mzlg02	.byte $4C,$46,$FF,$8B,$55,$00
mzlg03	.byte $8A,$8B,$52,$FF,$23,$67,$49,$00
mzlg10	.byte $FF,$A3,$77,$00
mzlg11	.byte $42,$9F,$7C,$00
mzlg12	.byte $4C,$46,$FF,$8E,$48,$B9,$66,$00
mzlg13	.byte $8C,$72,$FF,$23,$00
mzlg20	.byte $FF,$A3,$B7,$00
mzlg21	.byte $42,$9F,$84,$FF,$A9,$89,$67,$69,$00
mzlg22	.byte $46,$4C,$59,$6C,$98,$FF,$B9,$99,$87,$55,$48,$00
mzlg23	.byte $97,$B9,$FF,$36,$48,$55,$58,$23,$75,$78,$00
mzlg30	.byte $67,$00
mzlg31	.byte $42,$9F,$FF,$BB,$00
mzlg32	.byte $37,$38,$39,$3A,$3B,$3C,$FF,$A6,$69,$6F,$4A,$00
mzlg33	.byte $8C,$FF,$34,$35,$44,$58,$5A,$00

;***************************************
	.sbttl "Arrow Placement"
;***************************************
;* X Position, Y Position, Direction   *
;* 0 in X Position ends list           *
;***************************************
mzar00	.byte $A2,$00
		.byte $A3,$04
		.byte $B4,$00
		.byte $B5,$00
		.byte $B6,$00
		.byte $B7,$07
		.byte $A8,$07
		.byte $98,$03
		.byte $88,$07
		.byte $89,$00
		.byte $00
mzar01	.byte $74,$00
		.byte $76,$00
		.byte $89,$01
		.byte $86,$02
		.byte $A6,$00
		.byte $A7,$07
		.byte $98,$00
		.byte $9A,$00
		.byte $9B,$07
		.byte $00
mzar02	.byte $A6,$00
		.byte $A8,$07
		.byte $9D,$07
		.byte $5A,$01
		.byte $57,$02
		.byte $67,$02
		.byte $77,$00
		.byte $78,$00
		.byte $00
mzar03	.byte $A4,$00
		.byte $A5,$04
		.byte $9B,$03
		.byte $5B,$03
		.byte $39,$01
		.byte $35,$02
		.byte $55,$04
		.byte $00
mzar10	.byte $A2,$01
		.byte $76,$01
		.byte $84,$01
		.byte $A5,$01
		.byte $A8,$03
		.byte $00
mzar11	.byte $74,$00
		.byte $76,$00
		.byte $89,$01
		.byte $A7,$07
		.byte $99,$00
		.byte $9B,$07
		.byte $86,$02
		.byte $00
mzar12	.byte $A5,$01
		.byte $91,$03
		.byte $71,$00
		.byte $73,$00
		.byte $75,$04
		.byte $87,$00
		.byte $8A,$07
		.byte $6A,$01
		.byte $67,$02
		.byte $00
mzar13	.byte $A3,$08
		.byte $A4,$08
		.byte $A5,$08
		.byte $65,$08
		.byte $00
mzar20	.byte $A2,$08
		.byte $00
mzar21	.byte $74,$00
		.byte $76,$00
		.byte $87,$01
		.byte $A8,$07
		.byte $99,$00
		.byte $9B,$07
		.byte $7B,$03
		.byte $6C,$04
		.byte $AA,$06
		.byte $00
mzar22	.byte $A5,$01
		.byte $91,$05
		.byte $64,$02
		.byte $86,$01
		.byte $65,$07
		.byte $56,$04
		.byte $77,$02
		.byte $89,$00
		.byte $8D,$02
		.byte $9D,$01
		.byte $00
mzar23	.byte $A4,$01
		.byte $81,$03
		.byte $45,$06
		.byte $54,$01
		.byte $65,$00
		.byte $67,$00
		.byte $69,$03
		.byte $49,$07
		.byte $3A,$00
		.byte $6B,$02
		.byte $00
mzar30	.byte $A2,$01
		.byte $76,$01
		.byte $84,$01
		.byte $A5,$01
		.byte $A8,$03
		.byte $00
mzar31	.byte $74,$01
		.byte $83,$02
		.byte $81,$08
		.byte $64,$00
		.byte $69,$00
		.byte $98,$00
		.byte $AA,$08
		.byte $77,$00
		.byte $00
mzar32	.byte $A5,$01
		.byte $80,$03
		.byte $81,$03
		.byte $75,$04
		.byte $8C,$06
		.byte $67,$07
		.byte $49,$00
		.byte $6C,$00
		.byte $00
mzar33	.byte $A5,$00
		.byte $97,$00
		.byte $00
		
mzor00	.byte $89,$0C
		.byte $88,$09
		.byte $A8,$09
		.byte $B7,$0C
		.byte $B5,$0C
		.byte $A2,$09
		.byte $00
mzor01	.byte $8B,$09
		.byte $9A,$0C
		.byte $98,$0C
		.byte $A6,$0B
		.byte $86,$0A
		.byte $8A,$0B
		.byte $78,$0C
		.byte $76,$0C
		.byte $74,$09
		.byte $00
mzor02	.byte $78,$0C
		.byte $77,$0B
		.byte $67,$0B
		.byte $57,$0B
		.byte $47,$0A
		.byte $86,$0C
		.byte $84,$0C
		.byte $81,$09
		.byte $93,$0A
		.byte $00
		
;**************************************
	.sbttl "Trip Point Placement"
;**************************************
;* X Position, Y Position             *
;* 0 in X position ends list          *
;**************************************
mztr10	.byte $64,$83,$86,$87,$94,$B5,$78,$00
mztr11	.byte $76,$78,$98,$9A,$00
mztr12	.byte $61,$61,$54,$72,$74,$93,$00
mztr13	.byte $A6,$98,$66,$67,$68,$36,$37,$00
mztr20	.byte $64,$65,$94,$A6,$B5,$00
mztr21	.byte $77,$98,$99,$9A,$B7,$B9,$00
mztr22	.byte $8C,$8D,$8D,$6E,$00
mztr23	.byte $54,$64,$69,$49,$00
mztr30	.byte $B5,$B6,$00
mztr31	.byte $00
mztr32	.byte $8B,$8C,$8D,$00
mztr33	.byte $48,$4A,$00

;**************************************************
	.sbttl "Trip Point Action"
;**************************************************
;* Table of values for each trip point for each   *
;* maze and difcy level. Table is organized into  *
;* 128 byte groups, 1 group for each difcty level *
;* Each group contains entries for 4 mazes, 8     *
;* entries per maze, 3 bytes per entry.           *
;*                                                *
;* (X Position, Y Position, X vel index)          *
;*                                                *
;* X Pos = Position + Flags                       *
;*   Flag D7 = Only 1 release (bottom or left one)*
;**************************************************
;* difcty level 0 not included
;* difcty level 1
;* Maze 0
trtbl		.byte $88,$F6,$84
		.byte $83,$F8,$04
		.byte $09,$F8,$84
		.byte $09,$F8,$84
		.byte $87,$F9,$84
		.byte $89,$FB,$84
		.byte $8A,$F7,$84
		.byte $00,$00,$00
;* Maze 1
		.byte $84,$F7,$06
		.byte $85,$F7,$06
		.byte $88,$F9,$06
		.byte $8D,$F9,$86
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
;* Maze 2   
		.byte $01,$F6,$06
		.byte $06,$F6,$86
		.byte $87,$F5,$84
		.byte $87,$F7,$84
		.byte $82,$F7,$04
		.byte $81,$F9,$04
		.byte $00,$00,$00
		.byte $00,$00,$00
;* Maze 3
		.byte $89,$FA,$84
		.byte $8B,$F9,$84
		.byte $84,$F6,$04
		.byte $84,$F6,$04
		.byte $85,$F6,$04
		.byte $03,$F3,$04
		.byte $03,$F3,$04
		.byte $00,$00,$00
;* difcty level 2
;* Maze 0
		.byte $08,$F6,$84
		.byte $08,$F6,$84
		.byte $06,$F9,$84
		.byte $08,$FA,$84
		.byte $09,$FB,$84
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
;* Maze 1
		.byte $05,$F7,$06
		.byte $08,$F9,$01
		.byte $08,$F9,$01
		.byte $08,$F9,$01
		.byte $0C,$FB,$86
		.byte $0C,$FB,$86
		.byte $00,$00,$00
		.byte $00,$00,$00
;* Maze 2
		.byte $89,$F8,$04
		.byte $0A,$F8,$01
		.byte $0B,$F9,$00
		.byte $10,$F6,$84
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
;* Maze 3	
		.byte $06,$F5,$82
		.byte $04,$F6,$02
		.byte $0D,$F6,$84
		.byte $0C,$F4,$84
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
;* difcty level 3
;* Maze 0
		.byte $09,$FB,$84
		.byte $09,$FB,$84
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
;* Maze 1
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
;* Maze 2
		.byte $10,$F8,$84
		.byte $10,$F8,$84
		.byte $10,$F8,$84
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
;* Maze 3	
		.byte $0B,$F4,$84
		.byte $0D,$F4,$84
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
;* difcty level 4
;* Maze 0
		.byte $09,$FB,$84
		.byte $09,$FB,$84
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
;* Maze 1
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
;* Maze 2
		.byte $10,$F8,$84
		.byte $10,$F8,$84
		.byte $10,$F8,$84
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
;* Maze 3
		.byte $0B,$F4,$84
		.byte $0D,$F4,$84
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		.byte $00,$00,$00
		
ispeed	.byte $02,$05,$07,$00,$0E,$00,$15,$00
		.byte $03,$06,$08,$00,$10,$00,$18,$00
		.byte $03,$07,$09,$00,$12,$00,$1B,$00
		.byte $04,$08,$0A,$00,$14,$00,$1F,$00
		.byte $04,$09,$0B,$00,$16,$00,$22,$00
		
;************************************************
	.sbttl "Permanant/Dynamic Changes to Maze"
;************************************************
;* mzta - Permanant Changes                     *
;*        byte 1: Stamp Postion                 *
;*        byte 2: Stamp Number                  *
;*                                              *
;* mztd - Dynamic Changes                       *
;*        byte 1:
;*        byte 2:
;*        byte 3:
;* 	    byte 4:
;*        byte 5: 
;************************************************
mzta00	.byte $3B,$02
		.byte $3C,$05
		.byte $3D,$03
		.byte $4A,$07
		.byte $4B,$04
		.byte $00
		
mzta01	.byte $32,$07
		.byte $33,$05
		.byte $35,$02
		.byte $47,$07
		.byte $48,$03
		.byte $49,$07
		.byte $4A,$04
		.byte $00
		
mzta02	.byte $2E,$41
		.byte $43,$01
		.byte $44,$07
		.byte $45,$05
		.byte $57,$01
		.byte $58,$01
		.byte $59,$01
		.byte $5A,$03
		.byte $5B,$07
		.byte $5C,$02
		.byte $6B,$05
		.byte $6C,$01
		.byte $6D,$01
		.byte $6F,$01
		.byte $70,$02
		.byte $71,$04
		.byte $00
		
mzta03
mzta10
mzta11
mzta12
mzta13	.byte $00

mztd20	.byte $2C,$00,$08,$07,$01
		.byte $4B,$00,$08,$04,$03
		.byte $00
		
mzta20	.byte $3B,$05
		.byte $3C,$01
		.byte $3D,$03
		.byte $4C,$07
		.byte $00
		
mztd21	.byte $72,$00,$06,$01,$02
		.byte $87,$00,$06,$01,$03
		.byte $48,$00,$06,$02,$01
		.byte $49,$00,$06,$05,$01
		.byte $63,$00,$02,$06,$03
		.byte $00
		
mzta21	.byte $74,$02
		.byte $89,$03
		.byte $62,$01
		.byte $00
		
mztd22	.byte $71,$00,$06,$02,$01
		.byte $72,$00,$06,$05,$01
		.byte $75,$00,$06,$07,$01
		.byte $9A,$00,$10,$04,$06
		.byte $AF,$00,$10,$05,$06
		.byte $00
		
mzta22	.byte $34,$03
		.byte $47,$02
		.byte $48,$05
		.byte $49,$01
		.byte $56,$05
		.byte $57,$01
		.byte $5B,$07
		.byte $5C,$04
		.byte $5D,$03
		.byte $62,$01
		.byte $6C,$01
		.byte $6D,$01
		.byte $6F,$06
		.byte $73,$05
		.byte $74,$01
		.byte $75,$01
		.byte $84,$06
		.byte $85,$06
		.byte $87,$07
		.byte $88,$05
		.byte $89,$01
		.byte $8A,$01
		.byte $8B,$05
		.byte $99,$06
		.byte $B1,$04
		.byte $00
		
mztd23	.byte $7A,$00,$02,$03,$02
		.byte $7D,$05,$1C,$04,$05
		.byte $8D,$00,$02,$02,$03
		.byte $90,$05,$1C,$05,$04
		.byte $91,$00,$08,$07,$01
		.byte $B8,$00,$01,$07,$01
		.byte $7C,$00,$10,$01,$07
		.byte $8E,$12,$18,$01,$07
		.byte $8F,$0C,$18,$01,$07
		.byte $00
		
mzta23	.byte $31,$03
		.byte $42,$02
		.byte $43,$05
		.byte $44,$01
		.byte $55,$04
		.byte $56,$03
		.byte $6A,$02
		.byte $7C,$07
		.byte $8C,$01
		.byte $8E,$07
		.byte $8F,$07
		.byte $A0,$04
		.byte $A1,$01
		.byte $A3,$03
		.byte $A4,$07
		.byte $B4,$01
		.byte $00
		
mzta30	.byte $2A,$01
		.byte $3B,$01
		.byte $5A,$01
		.byte $5B,$01
		.byte $6A,$01
		.byte $6C,$02
mztd30	.byte $00

mzta31	.byte $33,$02
		.byte $34,$07
		.byte $35,$01
		.byte $36,$01
		.byte $48,$04
		.byte $74,$01
		.byte $75,$02
		.byte $A0,$01
		.byte $00
		
mztd31	.byte $4A,$00,$02,$07,$01
		.byte $00
		
mztd32	.byte $60,$00,$0C,$07,$01
		.byte $00
		
mzta32	.byte $4E,$01
		.byte $5B,$02
		.byte $62,$01
		.byte $63,$01
		.byte $6D,$01
		.byte $70,$04
		.byte $74,$01
		.byte $75,$01
		.byte $78,$07
		.byte $84,$01
		.byte $85,$01
		.byte $86,$01
		.byte $89,$01
		.byte $8A,$01
		.byte $8D,$01
		.byte $8E,$03
		.byte $99,$01
		.byte $9A,$01
		.byte $9B,$02
		.byte $9D,$05
		.byte $9E,$01
		.byte $9F,$02
		.byte $AE,$01
		.byte $AF,$02
		.byte $B0,$03
		.byte $B1,$07
		.byte $B3,$07
		.byte $B4,$04
		.byte $C3,$07
		.byte $C4,$06
		.byte $C5,$07
		.byte $C6,$07
		.byte $C7,$07
		.byte $C8,$06
		.byte $C9,$07
		.byte $CA,$06
		.byte $00
		
mzta33	.byte $43,$07
		.byte $51,$05
		.byte $55,$01
		.byte $56,$01
		.byte $64,$06
		.byte $7A,$01
		.byte $7B,$01
		.byte $92,$01
		.byte $93,$01
		.byte $A0,$01
		.byte $A1,$02
		.byte $A2,$07
		.byte $A4,$07
		.byte $A5,$01
		.byte $A6,$07
		.byte $AE,$06
		.byte $AF,$01
		.byte $B2,$03
		.byte $B3,$02
		.byte $B9,$01
		.byte $C1,$04
		.byte $C2,$01
		.byte $D7,$01
mztd33	.byte $00
		
;*****************************************
	.sbttl "One Way Walls"
;*****************************************
;* X LSB, Y LSB, 0 Ends List             *
;* cmright points to right,              *
;*       cmleft points to left           *
;*****************************************
mone00	.byte $00
mone01	.byte $92,$A3
		.byte $00
mone02	.byte $A5
mone03
mone10	.byte $00
mone11	.byte $FF,$B8
		.byte $00
mone12	.byte $FF,$A8
mone13
mone20 	.byte $00
mone21	.byte $FF,$B8
		.byte $00
mone22	.byte $FF,$A8
		.byte $00
mone23	.byte $FF,$A7
		.byte $00
mone30	.byte $FF,$B4
		.byte $FF,$A4
		.byte $00
mone31	.byte $FF,$B7
		.byte $00
mone32	.byte $8B
		.byte $FF,$8D
		.byte $00
mone33	.byte $29
		.byte $3B
		.byte $00
		
		
;************************************************
	.sbttl "Data Set for Laser Cannon Actions"
;************************************************
mcanst	.word 0
mcan01	.word mcp010,0
mcan30	.word mcp003,0
mcan31	.word mcp013,mcp113,0
mcan32	.word mcp023,mcp123,0
mcan33	.word mcp033,mcp133,mcp233,mcp333,0
mcan40	.word mcp004,0

;************************************************
	.sbttl "Code for Cannon Data Sets"
;************************************************
;* First Byte:                                  *
;*    D7-D6 = 00	Return to start of list       *
;*    D7-D6 = 01 	Move Gun to a new orientation *
;*                                              *
;*          D5-D3 = Angle of Gun:               *
;*                = 000 Top Right               *
;*                = 001 Middle Right            *
;*                = 010 Bottom Right            *
;*                = 011 Point Down              *
;*                = 100 Top Left                *
;*                = 101 Middle Left             *
;*                = 110 Bottom Left             *
;*                                              *
;*    D7-D6 = 10  Move Gun Location             *
;*                                              *
;*          D5-D0 = Frames of wait time/4       *
;*                = 0  Then Zero X and Y vel.   *
;*                >0   Then Velocities Follow   *
;*          Second Byte: X MSB Velocity         *
;*          Third Byte:  Y MSB Velocity         *
;*                                              *
;*    D7-D6 = 11  Pause                         *
;*    		D5-D0 = Frames of wait time/4       *
;************************************************
mcp010	.byte $00,$06,$80,$F6
		.byte $41,$18,	$49,$18,	$51,$18
		.byte $94,$10,$00,$80
		.byte $61,$18,	$69,$18,	$71,$18
		.byte $94,$F0,$00,$80
		.byte $40,        $60
		.byte $84,$F0,$00,$80
		.byte $59,$08
		.byte $88,$00,$FC,$80
		.byte $69,$08,    $40
		.byte $88,$00,$04,$80
		.byte $48
		.byte $84,$10,$00,$80
		.byte $00
mcp003	.byte $00,$09,$80,$F7
		.byte $81,$FC,$00
		.byte $75,$10,$75,$10,$54
		.byte $81,$04,$00
		.byte $55,$10,$55,$10,$74
		.byte $00
mcp013	.byte $00,$0B,$80,$F6
		.byte $75,$0D,$65,$0D,$75,$0D
		.byte $64
		.byte $D1
		.byte $00
mcp113	.byte $80,$0C,$80,$FA
		.byte $42
		.byte $C4
		.byte $53,$0C,$53,$0C
		.byte $00
mcp023	.byte $A0,$03,$80,$F7
		.byte $55,$0B,$45,$0B
		.byte $84,$08,$00,$80
		.byte $64
		.byte $84,$F8,$00,$80
		.byte $54,$D0
		.byte $00
mcp123	.byte $80,$0E,$80,$F7
		.byte $75,$0B,$65,$0B
		.byte $84,$F8,$00,$80
		.byte $44
		.byte $84,$08,$00,$80
		.byte $74,$D0
		.byte $00
mcp033	.byte $00,$04,$80,$FA
		.byte $D0
		.byte $55,$10,$55,$10
		.byte $C4
		.byte $00
mcp133	.byte $00,$06,$20,$F2
		.byte $53,$0A,$53,$14
		.byte $00
mcp233	.byte $80,$06,$40,$F6
		.byte $54
		.byte $81,$0C,$00
		.byte $81,$0C,$06
		.byte $81,$0C,$0C
		.byte $81,$0C,$06
		.byte $81,$0C,$00
		.byte $81,$0C,$FA
		.byte $81,$0C,$F4
		.byte $81,$0C,$FA
		.byte $81,$0C,$00
		.byte $81,$0C,$06
		.byte $81,$0C,$0C
		.byte $81,$0C,$06
		.byte $81,$0C,$00
		.byte $81,$0C,$FA
		.byte $81,$0C,$FC
		.byte $81,$0C,$FA
		.byte $81,$0C,$00
		.byte $81,$0C,$06
		.byte $81,$0C,$0C
		.byte $81,$0C,$06
		.byte $81,$0C,$00
		.byte $80
		.byte $64
		.byte $81,$F4,$00
		.byte $81,$F4,$FA
		.byte $81,$F4,$F4
		.byte $81,$F4,$FA
		.byte $81,$F4,$00
		.byte $81,$F4,$06
		.byte $81,$F4,$0C
		.byte $81,$F4,$06
		.byte $81,$F4,$00
		.byte $81,$F4,$FA
		.byte $81,$F4,$F4
		.byte $81,$F4,$FA
		.byte $81,$F4,$00
		.byte $81,$F4,$06
		.byte $81,$F4,$0C
		.byte $81,$F4,$06
		.byte $81,$F4,$00
		.byte $81,$F4,$FA
		.byte $81,$F4,$F4
		.byte $81,$F4,$FA
		.byte $81,$F4,$00
		.byte $80
		.byte $00
mcp333	.byte $E0,$08,$80,$F6
		.byte $81,$0C,$F4
		.byte $81,$0C,$FA
		.byte $81,$0C,$00
		.byte $81,$0C,$06
		.byte $81,$0C,$0C
		.byte $81,$0C,$06
		.byte $81,$0C,$00
		.byte $81,$0C,$FA
		.byte $81,$0C,$F4
		.byte $81,$0C,$FA
		.byte $81,$0C,$00
		.byte $80
		.byte $74
		.byte $81,$F4,$00
		.byte $81,$F4,$06
		.byte $81,$F4,$0C
		.byte $81,$F4,$06
		.byte $81,$F4,$00
		.byte $81,$F4,$FA
		.byte $81,$F4,$F4
		.byte $81,$F4,$FA
		.byte $81,$F4,$00
		.byte $81,$F4,$06
		.byte $81,$F4,$0C
		.byte $81,$F4,$06
		.byte $81,$F4,$00
		.byte $81,$F4,$FA
		.byte $81,$F4,$F4
		.byte $81,$F4,$FA
		.byte $81,$F4,$00
		.byte $81,$F4,$06
		.byte $81,$F4,$0C
		.byte $81,$F4,$06
		.byte $81,$F4,$00
		.byte $80
		.byte $44
		.byte $81,$0C,$00
		.byte $81,$0C,$FA
		.byte $81,$0C,$F4
		.byte $81,$0C,$FA
		.byte $81,$0C,$00
		.byte $81,$0C,$06
		.byte $81,$0C,$0C
		.byte $81,$0C,$06
		.byte $81,$0C,$00
		.byte $81,$0C,$FA
		.byte $00
mcp004	.byte $00,$09,$80,$F7
		.byte $86,$FA,$00
		.byte $75,$10,$75,$10
		.byte $86,$06,$00
		.byte $75,$10,$75,$10
		.byte $00
		
;**********************************************
	.sbttl "New Maze Objects"
;**********************************************
;* First comes clock and magic boots data     *
;*     Two Bytes for each wave                *
;*     0 in first byte means not present      *
;*     Otherwise X MSB and Y MSB respectively *
;**********************************************
mclock	.byte $00,$00,$00,$48
		.byte $00,$00,$00,$00
		.byte $00,$00,$00,$00
		.byte $8A,$00,$00,$00
		
		.byte $00,$00,$00,$00
		.byte $00,$00,$00,$28
		.byte $00,$00,$B9,$00
		.byte $00,$00,$62,$00
		
;**********************************************
	.sbttl "Stalactite Data"        
;**********************************************
;* X MSB and Y MSB of each stalactite         *
;* 0 ends list                                *
;**********************************************
tite11	.byte $76,$78,$00
tite12	.byte $61,$72,$74,$93,$54,$00
tite13	.byte $36,$37
tite20	.byte $00
tite21	.byte $77,$B7,$B9
tite22	.byte $00
tite23	.byte $54,$64
tite30
tite31
tite32	.byte $00
tite33	.byte $48,$4A,$26,$00
tite41	.byte $66
lock00	.byte $00

;***********************************************
	.sbttl "Lock and Key Data"  
;***********************************************
;* Color, X MSB Key, Y MSB Key,                *
;* X MSB Lock, Y MSB Lock.                     *
;* 0 ends list                                 *
;***********************************************
lock03	.byte $06,$9A,$28
		.byte $03,$4D,$29
		.byte $00
lock30	.byte $06,$67,$A5
		.byte $0C,$B6,$86
		.byte $03,$9A,$89
		.byte $00
lock32	.byte $03,$86,$61
		.byte $06,$6E,$63
		.byte $00
lock33	.byte $04,$35,$A4
		.byte $03,$2A,$B7
		.byte $0C,$9D,$39
		.byte $00
lock40	.byte $06,$67,$A5
		.byte $0C,$B6,$86
		.byte $0B,$81,$89
		.byte $00
lock41	.byte $01,$72,$7A
		.byte $02,$B7,$7A
		.byte $04,$6F,$7A
		.byte $00
		
;******************************************************
	.sbttl "Escape Pod Data"      
;******************************************************
;* 0 Means not on this wave                           *
;* 1 Means shows up but player doesn't have to use it *
;* 2 Means show up and doors don't open               *
;******************************************************
mpod		.byte 0,1,2,2

;******************************************************
	.sbttl "Transporter Data"    
;******************************************************
;* Color in D0-D3, Direction in D4(1=right),          *
;* X MSB, Y MSB                                       *
;* Color = 0 ends list                                *
;* Next comes packed transportability info            *
;*   $ff ends list                                    *
;******************************************************
tran00	.byte $00,$FF
tran02	.byte $06,$97,$16,$A3,$00
        .byte $00,$00,$00,$00,$00,$40,$EE
tran31	.byte $03,$6B,$13,$A7,$06,$AC,$16,$76,$04,$BA,$14,$5C
		.byte $00
		.byte $00,$C0,$FF,$03,$00,$0C,$EE
tran32	.byte $04,$9E,$14,$66
		.byte $00
		.byte $0C,$00,$FC,$03,$EE
tran33	.byte $0A,$99,$1A,$23,$05,$6B,$15,$95
hand00	.byte $00
		.byte $00,$EE
		
;******************************************************
	.sbttl "De Hand"
;******************************************************
;* X MSB, Y MSB, X Accordians, Y Accordians,          *
;* X degrees max, Y degree rest, Y degree Max,        *
;*  Visual Size X and Y                               *
;*  0 in X means not on this wave                     *
;******************************************************
hand12	.byte $09,$F8,$03,$02,$3F,$0B,$1F,$05,$03
hand30	.byte $06,$FA,$01,$02,$3F,$0B,$1F,$04,$03
hand33	.byte $02,$F4,$07,$02,$3F,$0B,$1F,$06,$03

;******************************************************
	.sbttl "Reactor Time Table"
;******************************************************
outime	.byte $30,$40,$40,$40
		.byte $38,$48,$50,$50
		.byte $40,$50,$30,$40
		.byte $40,$40,$50,$40
		
;******************************************************
	.sbttl "Generate New Maze"
;******************************************************
;* Generates a new maze, places all objects and such  *
;* in the maze.                                       *
;******************************************************
newmaze	jsr	placeall			;Place always objects
		lda	holmz				;if new maze, zero all statuses
		ifeq
			ldx	#nmfire-1
			begin
				sta	objst+zfire,X
				dex
			miend
			ldx	#nmrob-1
			begin
				sta	objst+zrobot,X
				dex
			miend
			ldx	#nmlock+nmkeys-1
			begin
				sta	objst+zlock,X
				dex
			miend
		endif
		lda	dif4mz			;Load in reactor time
		cmp	#$0F
		ifcs
			and	#03
			ora	#08
		endif
		tax
		lda	outime,X			;Need this now that reactor can be turned back off
		sta	reacst
		lda	#$FF
		sta	reacst+1			;Reactor status LSB
		lda	difcty
		cmp	#maxdif+2
		ifcs
			lda	#maxdif+1
		endif
		asl	A
		asl	A
		adc	maznum
		asl	A				;Everything in words
		tax					;Get init data for maze
		lda	mzinit,X			;Maze fireball source
		sta	temp1
		lda	mzinit+1,X			;Init data location
		sta	temp1+1
		ldy	#00				;Get the data
		lda	#01
		sta	objst+zreactor		;Turn on reactor
		lda	(temp1,Y)			;Reactor XL
		sta	objxl+zreactor
		iny	
		lda	(temp1,Y)			;Reactor XH
		sta	objxh+zreactor		
		iny	
		lda	(temp1,Y)			;Reactor YL
		sta	objyl+zreactor
		iny	
		lda	(temp1,Y)			;Reactor YH
		sta	objyh+zreactor
		ldx	#zfire			;Place for first fireball
		iny	
		begin					;Now do fireballs and robots
			lda	(temp1,Y)
			cmp	#$FF
			beq	?gnm10			;$ff ends list
			cmp	#$FE				;$fe ends fireballs, starts robots
			ifeq
				ldx	#zrobot
				iny
				lda	(temp1,Y)
			endif
			sta	objxl,X
			sta	oldxl,X
			lda	#00
			sta	ltcol,X			;Make sure collisions
			sta	rtcol,X
			sta	headcol,X
			iny	
			lda	(temp1,Y)
			sta	objxh,X			;Fireball XH
			iny	
			lda	(temp1,Y)
			sta	objyl,X			;Fireball YL
			iny	
			lda	(temp1,Y)
			sta	objyh,X			;Fireball YH
			iny	
			lda	(temp1,Y)
			jsr	incvel			;Check to see if this is an incrementing velocity
			sta	velxh,X
			cpx	#zrobot
			ifcs					;Might be a robot??
				sta	robdir-zrobot,X		;Save initial direction
				sta	robvel-zrobot,X		;Save his velocity too!
			endif
			iny	
			lda	(temp1,Y)
			jsr	incvel
			sta	velyh,X			;Fireball Y velocity
			bit	holmz				;Only old objects??
			ifpl
				inc	objst,X			;if first time around, turn on objects
			else
				lda	objst,X
				ifmi
?gnm9					lda	#00
					sta	objst,X			;Bring back to initial status
				else
					ora	limbo,X			;if alive or transporting when man died
					beq	?gnm9
					lda	#01
					sta	objst,X			;Bring back to initial status
				endif
			endif
			iny
			inx
		eqend					;Never!
?gnm10	ldx	#00				;Initialize Keys and Locks
		ldy	#00
		bit	holmz
		ifpl
			begin
				lda	(perm1,Y)			;Color of set
				beq	?gnm20			;No more
				sta	objst+zkeys,X
				sta	objst+zlock,X
				iny
				lda	(perm1,Y)
				jsr	crack
				sta	objxh+zkeys,X
				lda	perm5
				sta	objyh+zkeys,X
				iny	
				lda	(perm1,Y)
				jsr	crack
				sta	objxh+zlock,X
				lda	perm5
				sta	objyh+zlock,X
				iny	
				inx
			eqend					;Never
		endif
?gnm20	lda	#$80
		sta	holmz				;Hold init until next new maze
		lda	frame
		and	#$FC
		sta	frame				;Force start
		lda	#00				;Make sure no objects are in limbo at wave start
		ldx	#nmobj-1
		begin
			sta	limbo,X
			dex
		miend
		lda	#$83
		sta	objfrm			;Create new map buffer
		jmp	copym				;Do pre-map stuff
		
crack	sta	perm5+1			;Store away initial data
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		ora	#$F0				;Recreate Y coordinate
		sta	perm5
		lda	perm5+1
		and	#$0F
		clc	
		adc	#01				;X coordinate in accumulator
		rts	
		
incvel	cmp	#$70
		ifcs
			cmp	#$90				;Is this an incrementing velocity??
			ifcc					;yes
				cmp	#00
				ifmi
					and	#$7F			;Sign extend to all 8 bits
				else
					ora	#$80
				endif
				sta	perm4				;Amount to be added to velocity for each difficulty level
				iny
				lda	(temp1,Y)			;Base velocity
				stx	perm4+1
				ldx	incdif
				dex
				ifpl					;Don't add anything if difficulty zero
					begin
						clc
						adc	perm4
						dex	
					miend
				endif
				ldx	perm4+1			;Restore X
			endif
		endif
		rts
		
;********************************************8
	.sbttl "Copy Maze to RAM"
;********************************************
;* This routine copies the current maze to  *
;* RAM so that dynamic maze changes may take*
;* place.                                   *
;********************************************
copym		lda	maznum
		asl	A				;To words
		tay	
		lda	mazsrc+1,Y
		sta	mazpt+1
		lda	mazsrc,Y
		sta	mazpt				;Point to maze top
		ldy	#-1				;Will bump to 0 next time
		begin
			iny					;Next byte
			lda	(mazpt,Y)
			sta	mazer,Y			;Store into RAM
		miend
		rts
		
;********************************************
	.sbttl "Place Discs, Arrows and Trip Points"
;********************************************
;* This routine is used to  place all the   *
;* discs and arrows back in the maze        *
;* whenever needed without having to        *
;* re-init all other objects.               *
;********************************************			
placeall	lda	difcty
		cmp	#maxdif+2
		ifcs
			lda	#maxdif+1
		endif					;Maximize difcty at 4 (waves 17 thru 20)
		asl	A
		asl	A
		adc	maznum
		asl	A
		tax	
		lda	mzdc,X			;Disc Source Data
		sta	temp1				
		lda	mzdc+1,X
		sta	temp1+1
		lda	mzar,X			;Arrow Source Data
		sta	temp3				
		lda	mzar+1,X
		sta	temp3+1
		lda	mztr-8,X			;No Trip Pads on first four waves
		sta	temp4
		lda	mztr-7,X
		sta	temp4+1
		lda	mzlg,X			;Lightning Source Data
		sta	temp5
		lda	mzlg+1,X
		sta	temp5+1
		lda	mone,X			;One Way Arrows Source Data
		sta	temp6
		lda	mone+1,X
		sta	temp6+1
		lda	mtite-$0a,X			;No Stalactites on first 5 waves
		sta	temp7
		lda	mtite-9,X
		sta	temp7+1
		lda	mlock,X			;Lock Source Data
		sta	perm1
		lda	mlock+1,X
		sta	perm1+1
		lda	mtran,X			;Transporter Source Data
		sta	perm2
		lda	mtran+1,X
		sta	perm2+1
		lda	mhand-$0c,X			;No Hands on first six waves
		sta	perm3
		lda	mhand-$0b,X
		sta	perm3+1
		lda	#00
		ldx	#nmlsht+nmcann-1		;No laser shots or laser cannons
		begin
			sta	objst+zlsht,X
			dex
		miend
		ldx	#nmshot+nmtite+nmtran	;No robot shots or tites or transporters
		begin					;Also zero's player ship byte
			sta	objst+zshot,X
			dex
		miend
		ldx	#nmstuf-1
		begin
			sta	objst+zstuf,X		;No One of a Kinds
			dex
		miend
		ldx	#nmdisc+nmligh+nmfrfl+nmtrpp+nmonew+nmarow-1
		begin
			sta	objxh+zdisc,X		;No static objects
			dex
		miend
		ldx	#nmonew-1			;No one way signs
		begin
			sta	onewst,X
			dex
		miend
		lda	dif4mz
		tax					;Initialize clock and boots
		ldy	#00
		begin
			lda	mclock,X
			ifne
				jsr	crack
				sta	objxh+zstuf,Y
				lda	perm5
				sta	objyh+zstuf,Y
				lda	#01
				sta	objst+zstuf,Y
			endif
			txa
			clc
			adc	#numaze			;Access next set of 2 bytes per maze
			tax
			iny
			cpy	#02
		eqend
		lda	maznum			;Init Escape Pod
		cmp	#01
		ifeq
			ldx	difcty
			cpx	#numaze/4-1
			ifcs
				ldx	#numaze/4-1
			endif
			lda	mpod,X
			ifne
				lda	#01
			endif
			sta	objst+zstuf+2
			lda	#$10
			sta	objxh+zstuf+2
			lda	#$F6
			sta	objyh+zstuf+2
			lda	#00
			sta	epodfr
			sta	epodgr
		endif
		lda	dif4mz
		cmp	#06
		ifcs					;Init Hand, wave 6 and over
			ldy	#00
			lda	(perm3,Y)
			ifne
				lda	#01
				sta	objst+zstuf+3
				lda	#00
				sta	daccx				;Starts under the box
				lda	(perm3,Y)
				sta	objxh+zstuf+3
				iny	
				lda	(perm3,Y)
				sta	objyh+zstuf+3
				iny	
				lda	(perm3,Y)
				sta	naccx				;Number of X accordians
				iny	
				lda	(perm3,Y)
				sta	naccy				;Number of Y accordians
				iny	
				lda	(perm3,Y)
				sta	maccx				;Maximum X degrees
				iny	
				lda	(perm3,Y)
				sta	raccy				;Rest Y degrees
				sta	daccy				;Is what it starts at
				iny	
				lda	(perm3,Y)
				sta	maccy				;Maximum Y degrees
				iny	
				lda	(perm3,Y)
				sta	hxtend			;Visual Size, so draw accordian properly
				iny	
				lda	(perm3,Y)
				sta	hytend
			endif
		endif
		lda	dif4mz
		cmp	#05				;Initialize Stalactites, wave 5 and over
		ifcs
			ldx	#ztite
			ldy	#00
			begin
				lda	(temp7,Y)
				beq	?pla10
				jsr	crack
				sta	objxh,X
				lda	perm5
				sta	objyh,X
				inc	objst,X			;Turn on
				inx
				iny
			eqend					;Never
		endif
?pla10	ldx	#00
		ldy	#00
		begin
			lda	(temp6,Y)			;Init wall positions
			beq	?pla15				;End of data
			cmp	#$FF				;Points to left if $ff
			ifeq
				iny
				lda	#$80
			else_ne
				lda	#01				;Points to right
			endif
			sta	onewst,X
			lda	(temp6,Y)
			jsr	crack
			sta	objxh+zonew,X
			lda	perm5
			sta	objyh+zonew,X
			inx
			iny
		eqend					;Never
?pla15	ldy	#00
		ldx	#00
		begin
			lda	(temp1,Y)			;Init disc positions
			beq	?pla20			;End of data
			jsr	crack
			sta	objxh+zdisc,X
			lda	perm5
			sta	objyh+zdisc,X
			inx
			iny
		eqend					;Never
?pla20	ldy	#00
		ldx	#00
		begin
			lda	(temp3,Y)			;Init arrow positions
			beq	?pla25			;End of data
			jsr	crack
			sta	objxh+zarow,X
			lda	perm5
			sta	objyh+zarow,X
			iny
			lda	(temp3,Y)
			sta	ardir,X			;Direction of arrow
			inx
			iny
		eqend					;Never
?pla25	ldy	#00
		ldx	#00
		begin
			lda	(temp5,Y)			;Init lightning positions
			beq	?pla30
			cmp	#$FF
			ifeq					;Goto vertical ones
				ldx	#nmligh
				iny
				lda	(temp5,Y)
			endif
			jsr	crack
			sta	objxh+zligh,X
			lda	perm5
			sta	objyh+zligh,X
			inx
			iny
		eqend					;Never
?pla30	lda	dif4mz				;Trip Pads get drawn here
		cmp	#04
		bcc	?pla35
		ldy	#00
		ldx	#00
		begin
			lda	(temp4,Y)			;Init Point positions
			beq	?pla35			;End of data
			jsr	crack
			sta	objxh+ztrpp,X
			lda	perm5
			sta	objyh+ztrpp,X
			inx
			iny
		eqend					;Never
?pla35	lda	difcty
		cmp	#maxdif+2
		ifcs
			lda	#maxdif+1
		endif
		asl	A
		asl	A
		adc	maznum
		tay	
		ldx	mcan,Y			;Start of Laser Pointer Table
		ifne					;One level deeper than usual because action table for each item is of arbitrary length
			lda	#$FF
			sta	temp7
			begin
				lda	mcanst,X			;Start of pointer table
				sta	temp2
				lda	mcanst+1,X
				sta	temp2+1			;Pointer for cannon number temp7
				ldy	#00
				inx	
				inx	
				stx	temp7+1
				ldx	temp7
				inx	
				stx	temp7
				lda	(temp2,Y)			;X LSB
				iny	
				sta	objxl+zcann,X
				lda	(temp2,Y)			;X MSB
				iny	
				sta	objxh+zcann,X
				lda	(temp2,Y)			;Y LSB
				iny	
				sta	objyl+zcann,X
				lda	(temp2,Y)			;Y MSB
				iny	
				sta	objyh+zcann,X
				lda	#00
				sta	velxh+zcann,X		;No initial velocity
				sta	velyh+zcann,X
				sta	cannfr,X			;No wait before action
				sta	cannss,X			;No tube spinning
				sta	canngr,X			;Point to right barrel all the way out
				sta	canndf,X			;Difficulty of cannon
				lda	#04
				sta	cannin,X			;Points to start of table
				lda	#01
				sta	objst+zcann,X		;Turn it on!!
				lda	temp2
				sta	cannp1,X
				lda	temp2+1
				sta	cannp2,X
				ldx	temp7+1
				lda	mcanst+1,X			;Is there another one?
			eqend
		endif
		ldx	#00				;Initialize Transporter Booths
		stx	tspark
		stx	mestim
		stx	jmptim
		stx	fldcnt
		stx	jumprv			;Make sure idiot hints are initialized
		ldy	#00
		begin
			lda	(perm2,Y)			;Color of Pair
			beq	?pla40
			sta	objst+ztran,X
			iny	
			lda	(perm2,Y)
			jsr	crack
			sta	objxh+ztran,X		;X MSB of first
			lda	perm5
			sta	objyh+ztran,X		;Y MSB of first
			lda	#00
			sta	tranhi,X			;No sparkels in resting transporter
			iny	
			inx
		eqend					;Never
?pla40	iny	
		lda	#00				;Load compacted trasportability info
		sta	temp2
		begin
			lda	(perm2,Y)
			cmp	#$EE
			beq	?pla45			;$ee ends list
			sty	temp2+1
			ldx	#07
			begin
				ldy	temp2+1
				lda	(perm2,Y)
				and	transmsk,X			;Crack data
				ifne
					lda	#$80
				endif
				ldy	temp2
				sta	cktran,Y
				inc	temp2
				dex
			miend
			ldy	temp2+1
			iny
		eqend
?pla45	ldx	#ntrans-1
		begin
			lda	tspkls,X
			sta	ttran,X			;Stagger Transporter Sparkels
			dex
		miend
		rts
			
tspkls	.byte $06,$03,$05,$02,$05,$01,$04,$00

transmsk	.byte $80,$40,$20,$10,$08,$04,$02,$01

;***************************************************
	.sbttl "Initialize Maze Exit Arrows"
;***************************************************
newarrow	lda	dif4mz
		asl	A
		tax	
		cpx	#06
		ifcc
			lda	mzor,X
			sta	tempa
			lda	mzor+1,X
			sta	tempa+1
			lda	#00
			ldx	#nmarow-1
			begin
				sta	objxh+zarow,X
				dex
			miend
			tax	
			tay
			begin
				lda	(tempa,Y)			;Init arrow positions
				beq	?nar10			;End of data
				jsr	crack
				sta	objxh+zarow,X
				lda	perm5
				sta	objyh+zarow,X
				iny
				lda	(tempa,Y)
				sta	ardir,X			;Direction of arrow
				inx
				iny
			eqend
		endif
?nar10	rts	

;*************************************************
	.sbttl "Place All Objects"
;*************************************************
;* This routine will place and draw all objects  *
;* that are active into the map on the top of the*
;* screen.                                       *
;*                                               *
;* Inputs:	frame,objst,objxh,objyh              *
;*                                               *
;* Outputs: vectors to vglist                    *
;*                                               *
;* Uses:	temp6,vglist(see also locmap)        *
;*************************************************
domap		lda	#$20
		ldx	#$80
		jsr	vgadd2			;All vector JSRL's assume that the generator is
		lda	#00
		sta	vgbrit
		ldx	#$72
		jsr	vgadd2			;Scaled and centered
		lda	objst				;Display man if alive
		ifpl
			ifne
				ldx	#00
				jsr	locmap
			endif
		endif
		inc	objfrm
		lda	objfrm			;Keep track of which buffer to display or update
		and	#$7F
		cmp	#04
		ifcs
			lda	objfrm
			and	#$80
			eor	#$80
			sta	objfrm
			jsr	newobuf			;Update Buffer
		endif
		ldy	#00
		lda	objfrm
		ifmi
			ldy	#02
		endif
		lda	objsr-4,Y			;Call relevant buffer
		ldx	objsr-3,Y
		jsr	vgadd2
		ldy	#nmkeys-1			;Draw keys next to map
		sty	temp9
		begin
			lda	objst+zkeys,Y		;In Rex's Pocket?
			cmp	#$10
			ifcs
				jsr	vgcntr
				lda	#00
				ldx	#$71
				jsr	vgadd2
				ldy	temp9
				lda	topkeypos,Y				;X to key position
				ldx	#$27					;Y to key position
				ldy	#00
				jsr	$vgvtr
				ldy	temp9
				lda	objst+zkeys,Y
				and	#$0F
				ora	#$E0
				ldx	#lock7
				jsr	vgadd2
				lda	glock+2
				ldx	glock+3
				jsr	vgadd2
			endif
			dec	temp9
			ldy	temp9
		miend
		rts	
		
topkeypos	.byte $CC,$D2,$D8,$DE

		jsrl(crsbuf)
		jsrl(crsbuf+$800)
	
objsr		jsrl(crman)
		jsrl(crreac)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crfire)
		jsrl(crlsht)
		jsrl(crlsht)
		jsrl(crlsht)
		jsrl(crlsht)
		jsrl(crlsht)
		jsrl(crlsht)
		jsrl(crlsht)
		jsrl(crlsht)
		jsrl(crcann)
		jsrl(crcann)
		jsrl(crcann)
		jsrl(crcann)
		jsrl(crrob)
		jsrl(crrob)
		jsrl(crrob)
		jsrl(crrob)
		jsrl(crrob)
		jsrl(crrob)
		jsrl(crrob)
		jsrl(crrob)
		jsrl(crrob)
		jsrl(crrob)

;*****************************************************
	.sbttl "Locate in Map"
;*****************************************************
;* This routine places and object in the map at the  *
;* top of the screen.                                *
;*                                                   *
;* Input:	(X) = object number                      *
;*                                                   *
;* Output:	vector drawn to map position             *
;*                                                   *
;* Uses:	temp1, temp2, xcomp                      *
;*****************************************************
locmap	stx	perm1				;Save X
		lda	#$4B				;Assumed centered and scaled
		sta	xcomp				;Assume vgbrit is zero
		lda	#-1
		sta	xcomp+1			;Position of upper left corner
		lda	#$D2				;$C0 + fudge of 12
		sta	xcomp+2
		lda	#01
		sta	xcomp+3
		ldx	perm1
		lda	objxl,X
		sta	temp2
		lda	objxh,X
		ldy	#03
		begin
			cmp	#$80
			ror	A
			ror	temp2
			dey
		miend					;Shift 4 times
		sta	temp2+1			;High byte
		clc	
		lda	xcomp
		adc	temp2
		sta	xcomp
		lda	xcomp+1
		adc	temp2+1
		sta	xcomp+1			;Corrected X position
		lda	objyl,X
		sta	temp2
		lda	objyh,X
		sta	temp2+1
		ldy	#03				;Divide 4 times
		begin
			cmp	#$80
			ror	A
			ror	temp2
			dey
		miend
		sta	temp2+1
		lda	xcomp+2
		clc	
		adc	temp2
		sta	xcomp+2
		lda	xcomp+3
		adc	temp2+1
		sta	xcomp+3
		jsr	vgvtr2
		lda	perm1
		asl	A
		tay	
		lda	objsr,Y
		ldx	objsr+1,Y
		jsr	vgadd2			;Add the JSRL to the cross
		ldx	perm1
		rts	
		
;************************************************
	.sbttl "Create new cross buffer for radar"
;************************************************
newobuf	lda	vglist+1
		pha	
		lda	vglist
		pha	
		lda	objfrm			;Set vglist to appropriate buffer
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		ora	#crsbuf/$100
		sta	vglist+1
		lda	#crsbuf&$ff
		sta	vglist
		ldx	#zshot-1
		begin
			lda	objst,X
			ifne
				ifpl
					jsr	locmap
				endif
			endif
			dex
			cpx	#01
		eqend
		lda	objfrm
		ifpl
			ldx	#01
			jsr	locmap
		endif
		ldx	#$C0
		jsr	vgadd2			;RTSL at end of buffer
		pla	
		sta	vglist
		pla	
		sta	vglist+1			;Restore pointer to vgram
		rts	
		
;********************************************
	.sbttl "Draw Map from Bit Map"    
;********************************************
;* Draws the map on the top of the screen.  *
;* Creates a bit map using 'build' and then *
;* uses ones or zeros to create the map.    *
;*                                          *
;* Uses: temp1,temp2,temp3,temp4,temp5,temp6*
;*       temp7,temp8,temp9,A,X,Y            *
;********************************************
map		jsr	newmaze			;Init next maze, copy maze to RAM - This also copies maze to page 1 RAM too
		jsr	trapal
		lda	#mapbuf&$ff
		sta	vglist
		lda	#mapbuf/$100
		sta	vglist+1
		lda	#02
		ldy	#00
		sty	linen				;Line 0
		sty	temp3+2
		sty	temp3+3			;Set future Y component to 0 for mapline
		sty	temp7				;Set flag for H
		jsr	vgscal			;Set scale
		lda	#00				;Set to black, game will change later
		ldx	#$60
		jsr	vgadd2			;Set color of map
		jsr	getpt2			;Get maze source pointer
		lda	#$60
		sta	xcomp				;Now set up map position
		lda	#-1
		sta	xcomp+1			;XH
		lda	#$90
		sta	xcomp+2			;YL
		lda	#01
		sta	xcomp+3			;YH
		begin
			jsr	clr8
			jsr	vgcntr			;Center for position
			lda	#04
			sta	temp5				;Will do 3 before another center
			ldy	#00
			ldx	#xcomp			;Position this line
			jsr	advec2			;Do vgvctr and clear temp3
			begin
				ldy	#00				;Restore Y
				jsr	build				;Build first line of this maze
				ifne					;Either maze of end
					jsr	shift				;Look at first bit
					ifcs
						jsr	ones
					else
						jsr	zeros
					endif
				endif
				ldy	#01
				lda	(mazpt,Y)			;Look at first stamp, next line
				bmi	done3				;Is end of maze
				lda	mazpt
				clc	
				adc	#05				;Pass blank ends
				sta	mazpt
				ifcs
					inc	mazpt+1
				endif
				dec	temp5
				bmi	?dmb10			;Done here, reposition
				lda	#-$10
				sta	temp8+2
				lda	#$FF
				sta	temp8+3
				ldx	#temp8
				ldy	#00
				jsr	advec2
				jsr	clr8
			neend
?dmb10	 	lda	xcomp+2			;Point next line
			sec	
			sbc	#$50
			sta	xcomp+2
			ifcc
				dec	xcomp+3
			endif
			lda	#00
		neend					;Always loop
done3		lda	#00				;Done with H lines... now do V lines
		sta	temp3
		sta	temp3+1			;X component must be 0
		lda	#$80
		sta	temp7				;V flag
		jsr	getpt2			;Repoint to top of data
		lda	mazpt
		sta	temp6
		lda	mazpt+1
		sta	temp6+1			;Save this for next line
		lda	#$68
		sta	xcomp
		lda	#-1
		sta	xcomp+1
		lda	#$98
		sta	xcomp+2
		lda	#01
		sta	xcomp+3			;Position for vert lines
		jsr	clr8				;A=0 on return
		sta	temp5				;Force first line to a center position
		begin
			ldy	#00
			lda	temp6				;Reload source pointer
			sta	mazpt
			lda	temp6+1
			sta	mazpt+1			;xfer to mazpt
			jsr	buildv			;Build vert line
			jsr	shift			
			ifne					;Assume no short little line on top
				php					;Save carrry
				dec	temp5				;Found a good one?
				ifmi					;We need to position here
					jsr	vgcntr
					lda	#04
					sta	temp5				;Reset count
					ldx	#xcomp			;Position
				else					;else just add return vector
					ldx	#temp8
				endif
				ldy	#00				;Don't show it!!
				jsr	advec2			;Position for this
				jsr	clr8
				plp					;Restore carry
				ifcs
					jsr	ones
				else
					jsr	zeros
				endif
			endif
			lda	temp8
			clc	
			adc	#$10
			sta	temp8
			ifcs
				inc	temp8+1
			endif
			lda	xcomp
			clc	
			adc	#$10
			sta	xcomp
			ifcs
				inc	xcomp+1
			endif					;Point to next line
			lda	temp6
			clc	
			adc	#01
			sta	temp6
			ifcs
				inc	temp6+1
			endif
			ldy	#00
			lda	(temp6,Y)			;End of maze
		eqend
		lda	#(retbuf/$100)&$ff
		ldx	#retbuf&$ff
		jsr	vgjsrl
		lda	#(timbuf/$100)&$ff
		ldx	#timbuf&$ff
		jmp	vgjmpl
		
;****************************************************
	.sbttl "Build Line"
;****************************************************
build		lda	#27d				;28 Max Across
		sta	temp2
		lda	(mazpt,Y)
		ifeq
			rts					;Skip if no line
		endif
		ifpl
			begin
				and	#$0F				;Drop any special bits
				tax
				lda	mapbits,X
				jsr	shiftin
				dec	temp2				;Another one down
				iny
				lda	(mazpt,Y)
			eqend
			lda	temp2				;Did we do all?
			ifpl					;Nope
				begin
?bl4					lda	#00
					jsr	shiftin			;0 fill
					dec	temp2
				miend
			endif
			lda	#00				;Flag for + return
		endif					;Returns - if skipped
		ifpl
			tya
			clc
			adc	mazpt				;Update source pointer
			sta	mazpt
			ifcs
				inc	mazpt+1
			endif
			lda	#01				;Set draw flag
		endif
		rts
		
;******************************************
	.sbttl "Build V line" 
;******************************************
buildv	ldx	maznum
		lda	mazdep,X			;Count of lines
		sta	temp2
		ldy	#00
		lda	(mazpt,Y)
		begin
			and	#$0F				;Drop special bits
			tax	
			lda	mapvbt,X			;Bit pattern vert
			jsr	shiftin
			ldx	maznum
			lda	mazpt
			clc	
			adc	mazlen,X			;Add length to next line
			sta	mazpt
			ifcs
				inc	mazpt+1
			endif
			lda	(mazpt,Y)			;Next stamp
			dec	temp2
		eqend					;All lines
		lda	#27d				;Force shift left
		sec	
		sbc	mazdep,X
		sta	temp2
		bpl	?bl4
		rts	
		
;*****************************************
	.sbttl "Shift in to Map"
;*****************************************
;* A = Shift pattern                     *
;*****************************************
shiftin	ldx	#01				;Shift in 2 bits
		begin
			asl	A
			rol	map6
			jsr	rol50
			dex
		miend
		rts	
		
;*****************************************
	.sbttl "Shift 'Bit Map'"          
;*****************************************
shift		asl	map6				;Next shift
		jsr	rol50
		lda	map0
		ora	map1
		ora	map2
		ora	map3
		ora	map4
		ora	map5
		ora	map6
		rts	
		
;*****************************************
	.sbttl "Ones Routine" 
;*****************************************
ones		begin				;Entered with a known 1
			jsr	add8			;Add 8 to temp3
			jsr	shift
			ifeq
				ifcs				;Was 1 on last carry out
					jsr	add8
				endif
				ldy	#$C0
				jmp	advec
			endif
			ifcc				;We changed
				ldy	#$C0			;So output this vector
				jsr	advec
				clc				;And do another routine
				bcc	zeros			;Change routines
			endif
		ccend
;*****************************************
	.sbttl "Zeros Routine"
;*****************************************
zeros		begin
			jsr	add8
			jsr	shift			;Next bit
			ifeq
				bcs	?zer8			;Might have been one left
				ldy	#00
				jmp	advec
			endif
			ifcs				;A change occured
?zer8				ldy	#00
				jsr	advec
				sec
				bcs	ones			;Go do ones routine
			endif
		csend				;Always loop!!

;*****************************************
	.sbttl "Bump Maze Pointer"
;*****************************************
;* Bump indirect pointer for maze draw   *
;* routines and leave Y=0 for indirect   *
;* use.                                  *
;*****************************************
incmaz	inc	mazpt
		ifeq
			inc	mazpt+1
		endif
		ldy	#00				;Always 0
		rts
		
;*****************************************
	.sbttl "Add vector to List"  
;*****************************************
;* Adds a vector to the vglist. Length   *
;* of vector is in temp3(4). If the      *
;* vector is under length of +/-32d then *
;* a short vector will be generated.     *
;*****************************************
;*** (Y) = 80 to draw, 0 Not to draw   ***
;*****************************************
advec		sty	vgbrit
		lda	temp8
		sec	
		sbc	temp3				;Running total for return
		sta	temp8				;Subtract to create return vector
		lda	temp8+1
		sbc	temp3+1
		sta	temp8+1
		sec	
		lda	temp8+2
		sbc	temp3+2
		sta	temp8+2
		lda	temp8+3
		sbc	temp3+3
		sta	temp8+3			;Total updated for return later
		lda	temp3
		sta	temp1
		lda	temp3+1
		sta	temp1+1
		ifmi					;Need absolute value
			jsr	dblneg			;Get positive number
		endif
		ifeq					;Testing temp1+1
			lda	temp1
			cmp	#32d				;Is X less than 32d?
			ifcc					;yep!
				lda	temp3+2
				sta	temp1
				lda	temp3+3
				sta	temp1+1
				ifmi				;Now check Y (absolute value)
					jsr	dblneg
				endif
				ifeq
					lda	temp1
					cmp	#32d
					ifcc				;Both X and Y less than 32d
						lda	temp3+2		;Output vector (get del Y)
						cmp	#$80
						ror	A			;/2
						and	#$1F
						ora	#$40			;Add instruction
						tax
						lda	temp3			;delta X
						cmp	#$80
						ror	A
						and	#$1F
						ora	vgbrit		;Add brightness
						jsr	vgadd2
						jmp	?av7
					endif
				endif
			endif
		endif
		ldx	#temp3			;Vector data location
advec2	sty	vgbrit
		sta	watchdog			;Kick the doggie!!
		jsr	vgvctr			;Add the vector
?av7		lda	#00
		ldx	#00
		bit	temp7
		ifmi
			ldx	#02
		endif
		sta	temp3,X
		sta	temp3+1,X			;Clear for next vector
		rts
		
;**********************************************
	.sbttl "Add 8 to temp3(2)"             
;**********************************************
add8		bit	temp7				;V?
		ifpl					;Doing H
			lda	temp3				;Add half segment
			clc
			adc	#08
			sta	temp3
			ifcs
				inc	temp3+1
			endif
		else					;Do V line
			lda	temp3+2
			sec
			sbc	#08
			sta	temp3+2
			ifcc
				dec	temp3+3
			endif
		endif
		rts
		
;***********************************************
	.sbttl "Get Map Data Pointer"
;***********************************************
getpt2	jsr	getptr			;Points to start of data
		lda	mazpt				;Now point past top blank lines
		clc	
		adc	#m0u1-maz0+4		;Should point to first real data
		sta	mazpt
		ifcs
			inc	mazpt+1
		endif
		rts
		
;***********************************************
	.sbttl "Clear temp8(4)"             
;***********************************************	
clr8		lda	#00
		sta	temp8
		sta	temp8+1
		sta	temp8+2
		sta	temp8+3			;Clear return vector
		rts
		
;***********************************************
	.sbttl "ROL50"
;***********************************************
rol50		rol	map5
		rol	map4
		rol	map3
		rol	map2
		rol	map1
		rol	map0
		rts	

	.nocodes		;So we dont have list file buffer overflows
	.fill $4000-*
	.end
	
.export drawm,map,domap,copym,trtbl,ispeed,newarrow,trap