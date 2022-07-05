 .locallabelchar "?"
#include "logic.ah"
#include "vector.ah"
#include "mh_vrom.exp"
#include "mh_alpha.exp"
#include "g_command.ah"
 .module aux2
;********************************************
;* Major Havoc Auxiliary Program Page 2     *
;********************************************
	.title "TWMAZE - Maze Routines"
	.sbttl "Globals"
	
	.org $2000


cksum9	.byte $BD

;************************************************
	.sbttl "Init RAM and VRAM"
;************************************************
init		ldx	#00
		stx	vgreset				;Stop the Vector Generator
		begin
			lda	#$C0					;Set all to RTSL
			sta	vecram,X
			sta	vecram+$100,X
			sta	vecram+$200,X
			sta	vecram+$300,X
			sta	vecram+$400,X
			sta	vecram+$500,X
			sta	vecram+$600,X
			sta	vecram+$700,X
			sta	vecram+$800,X
			sta	vecram+$900,X
			sta	vecram+$A00,X
			sta	vecram+$B00,X
			sta	vecram+$C00,X
			sta	vecram+$D00,X
			sta	vecram+$E00,X
			sta	vecram+$F00,X
			lda	#00
			sta	0,X
			cpx	#$E0
			ifcc
				sta	$100,X				;Clear most of page 1
			endif
			sta	$200,X
			sta	$300,X
			sta	$400,X
			sta	$500,X
			sta	$600,X
			sta	$700,X
			sta	$800,X
			
			;*****************************************************************
				.sbttl "Version 3 Correction"
			;*****************************************************************
			;* Major Havoc, Ver 1.2 to 1.3 Bug Fix... There is a cleaner way *
			;* but this changes only 1 part.                                 *
			;*****************************************************************
				jmp	fixit
patchreturn
			;*****************************************************************
			;* sta $900,X     ; This instruction fixed in the patch          *
			;********** End Patch Area ***************************************
			
			inx
		eqend
		ldx	#30d-1			;Copy all, wash later
		begin
			lda	dinit,X			;Get default initials
			sta	initl,X
			dex
		miend					;Copy default initials in
		ldx	#40d-1			;Move Scores too
		begin
			lda	dfscore,X			;Default Scores
			sta	hscore,X
			dex
		miend
		lda	#$0D				;Pre-Set to..
		;* Both Processors On
		;* Gig Lamp On
		;* Inverts Clear
		;* Player select to 0
		sta	plysel
		sta	out1s				;Set shadow to 0 too
		;* Init VG Buffers
		lda	#01
		sta	vecram
		lda	#$E4
		sta	vecram+1			;Put in initial jump
		lda	#$20
		sta	vecram+3			;And a halt just in case
		sta	vecram+$803			;Here too
		jsr	nextmz			;Set up initial params
		lda	#$FF
		sta	manstat			;Force a restart
		sta	frame				;Start right away
		lda	#$80
		sta	mzgrnd			;Start on the ground
		lda	#11d
		ldx	#05
		begin
			sta	pl1last,X			;Init first entry to AAA
			dex
		miend
scbint	ldx	#scoint-scoin1		;Init score buffer data
		begin
			lda	scoin1,X
			sta	scobuf,X
			sta	scobuf2,X			;Player 2 score buffer
			cpx	#retint-retini+1		;Score area
			ifcc					;Time to do it!!
				lda	retini,X
				sta	retbuf,X			;Reactor time buffer
				lda	timini,X
				sta	timbuf,X			;Maze time buffer
			endif
			dex
		miend
		lda	#mancol2+$a0			;Init Score headings
		sta	scobuf2+2			;Change color on player 2
		lda	#$10		
		sta	scobuf2+8			;Position slightly different
		lda	#01
		sta	scobuf2+9			;To right of screen
init2		bit	gamest			;Attract??
		ifpl					;No, wrong aftershave
			lda	#00
			sta	frame
			sta	frame+1
		endif
;* Fall Through
;***********************************************
	.sbttl "Game Play Start Init!"
;***********************************************		
gminit	lda	#$50
		sta	bonusa			;Start with 5000 bonus
		lda	#$18
		sta	nxtdly			;Wait until first launch
		lda	#$C0
		sta	olmznm			;So it will start new
		sta	target			;Shots target
		sta	tactde			;Want tact first time
		lda	#01
		sta	nxtptr			;First entry again!
		;**************************************************
		jsr	newbrk			;Restore breakout bricks
		lda	#00				;Init stuff
		sta	colcnt			;Reset collision count
		sta	bronce			;Clear bonus flag
		sta	statst			;Make sure station is off
		sta	lauen				
		sta	holmz				;Don't hold old objects
		sta	tact				;Make sure display is off
		sta	maznum			
		sta	rearview			;Stars forward please
		sta	tstat				;Restart tunnel first time only
		sta	difcty			;And start difcty
		sta	scbdis+3			;Clear score display
		sta	updflg
		sta	updint			;Clear table
		ldx	#nmobj-1
		begin
			sta	sobjst,X			;Clear old enemy shots
			sta	objst,X			;Clear old ship shots
			;* And all other objects as well
			cpx	#hitpts-scrbuf
			ifcc
				bit	gamest			;Playing??
				ifpl					;Do only if new game
					sta	scrbuf,X			;Clear old scores
				endif
			endif
			dex
		miend					;Clear score and score buffers
		lda	#trtbl&$ff
		sta	trinds			;Init trip point buffer
		lda	#trtbl&$ff00/$100	
		sta	trinds+1
		lda	#$32
		sta	tcount			;Next tunnel init
		lda	#-3				;For man running to ship
		sta	xmot+3			;Also man's initial Y MSB
		lda	#-4
		sta	xmot+1			;Start at Left (X MSB)
		lda	#$90
		sta	xmot				;X LSB
		lda	#$6A
		sta	xmot+2			;Man's Y LSB
		lda	#27d				;Standing with arms crossed
		sta	piccur
		lda	#$40
		sta	mzgame			;Start in tube
		sta	mzgms				;Save shadow too!
		jmp	initshp			;Do initshp and end
		
;*****************************************************
	.sbttl "New Score - Bonus for Add-a-Coin Start"
;*****************************************************
newscore	ldy	#00				;Guess player 1
		lda	player
		ifne					;Is player 1
			ldy	#score2-score
		endif
		lda	wrplvl			;Player starting within a warp?
		asl	A				;13 possible starting levels +3 unused byte
		asl	A
		asl	A
		asl	A
		adc	dif4mz
		tax	
		lda	stsch,X			;MSB of start code
		sta	score+2,Y
		lda	#$80
		sta	scrflg			;Signal Change
		rts	
		
stsch		.byte $00,$05,$10,$15,$20,$25,$30,$35,$40,$45,$50,$55,$60,$00,$00,$00
		.byte $00,$05,$10,$25,$30,$35,$40,$45,$50,$55,$60,$65,$70,$00,$00,$00
		.byte $00,$05,$10,$25,$30,$35,$40,$45,$60,$65,$70,$75,$80,$00,$00,$00
		.byte $00,$05,$10,$25,$30,$35,$40,$45,$60,$70,$75,$80,$85,$00,$00,$00
		.byte $00,$05,$10,$25,$30,$35,$40,$45,$60,$70,$75,$80,$95,$00,$00,$00

;**************************************
	.sbttl "New Breakout Bricks"
;**************************************
newbrk	lda	#$FC
		ldx	#02
		begin
			sta	brick,X			;Breakout bricks on
			dex
		miend
		rts
		
;************************************************
;* Score Area Init Data                         *
;* Bright $0e                                   *
;* CNTR                                         *
;* VCTR -260,1C0,0                              *
;************************************************	
scoin1	.byte $50,$72,mancol+$e0,live7,$20,$80,$60,$02,$40,$1D,$00,$C0
scoint

retini	.byte $40,$71,$F3,$60,$20,$80,$C0,$00,$E0,$00
		jsrl(char_space)

retint
timini	.byte $40,$71,$00,$60,$20,$80,$C0,$00,$40,$00
		jsrl(char_space)
		
;* High Score Table Default Data *
dinit		.byte $0B,$1F,$15,$0B,$1F,$15,$0B,$1F,$15
		.byte $19,$1C,$1C,$17,$0F,$0D,$17,$0A,$12
		.byte $0E,$0F,$1D,$11,$0A,$0D,$0C,$0B,$21
		.byte $1D,$23,$18
		
dfscore	.byte $32,$45,$09,$00,$17,$82,$08,$00,$85,$72,$07,$00
		.byte $22,$11,$06,$00,$72,$70,$05,$00,$52,$58,$04,$00
		.byte $54,$15,$03,$00,$65,$93,$02,$00,$09,$81,$00,$00
		.byte $40,$52,$00,$00

;************************************************************
	.title "TWSphere"
	.sbttl "enem0 -  Master Control Routine"
;************************************************************
;* Constants
nweb 	=	25d		;Number of web spinners
nsph	=	10d
nbug	=	8
shots	=	8		;nmsshots number of player shots
stat	=	$62		;Stat opcode
statsp =	$63
defint =	$f0		;All purpose maze intensity
preptim =	$40		;One second of prepare time

; RAM Equates

?localpc = *

 .org $600
 
difmx3	.block	1		;Difficulty maxing out at number of waves
six		.block	1		;0 to 5 counter
sphrxl	.block	1		;Center of sphere rotation position
sphrxh	.block	1		
sphryl	.block	1
sphryh	.block	1
sphrad	.block	1		;Radius of sphere orbits
sphrst	.block	1		;0 or 1 depending on rotation post
sphscl	.block	1		;Sphere scale LSB
sphsch	.block	1		;Sphere scale MSB
sphcol	.block	1		;Sphere Color
statdeg	.block	1		;Degrees of station
cntrdeg	.block	1		;Degrees of center of rotation
shipadd	.block	1		;Ship offset for randomizing bug motion
corxl		.block	1		;Corrected ship X LSB for tracking
corxh		.block	1		;Corrected ship X MSB for tracking
bugxv		.block	1		;Bug X velocity
bugyv		.block	1		;Bug Y velocity
bugim		.block	1		;Bug picture number
websav	.block	nweb		;Save spinner statuses through orr code
webstim	.block	nweb		;When do the dragonflies drop?
fishmys	.block 	1		;Mystery Value

 .org ?localpc


enem0		lda	init3
		ifmi
			lda	#00
			sta	init3
			jmp	zero0
		endif
		lda	webssta+nsph-1			;My cue for return from orr code
		ifeq
			jsr	back					;Restore all statuses
		endif
		ldy	#00
		sty	fishmys
		ldx	#nweb-1
		begin
			lda	webssta,X
			sta	websav,X
			ifne
				iny
			endif
			dex
		miend
		cpy	#01
		ifeq
			lda	shipst
			ifpl
				ifne
?enm10				jmp	noneleft				;All dying or dead
				endif
			endif
		endif
		jsr	stdisp
		lda	websnum
		sta	webscur
		begin
			jsr	hvdisp
			dec	webscur
		miend
		lda	websnum
		sta	webscur
		begin
			jsr	fsdisp
			dec	webscur
		miend
		lda	frame
		and	#$3F
		ifeq
			lda	bonusa
			sed
			sec
			sbc	#01				;Decrement bonus on the basis of time
			sta	bonusa
			cld
			ifeq
				jsr	back2
				beq	?enm10
			endif
		endif
		lda	six					;Main 'six' counter section, counts 0-5
		clc	
		adc	#01
		cmp	#06
		ifeq
			lda	#00				;If it is at 6, reset it
		endif
		sta	six
		ldy	difmx3
		lda	frame
		and	?enm120,Y
		ifeq
			jsr	getrand
			and	#03
			cpy	#02
			ifcc
				tay	
				lda	?enm100,Y
				sta	corxh
				lda	?enm110,Y
			else
				tay
				lda	?enm105,Y
				sta	corxh
				lda	?enm115,Y
			endif
			sta	corxl
		endif
		lda	sndcue,abs
		ifeq
			lda	#$20
			sta	sndcue,abs
			lda	#snd_c5				;Bonus Tick Sound
			jsr	dosound
		endif
		lda	sphrad
		cmp	#$80
		ifcs
			jsr	orbit2
		else
			cmp	#$30
			ifcc
				lda	sndcue+1,abs
				ifeq
					lda	#08
					sta	sndcue+1,abs
					lda	#snd_d1
					jsr	dosound			;Fish Hatch Sound
				endif
			endif
		endif
		rts	

?enm100	.byte $02,$03,$05,$06
?enm105	.byte $03,$04,$05,$06
?enm110	.byte $F0,$20,$E0,$10
?enm115	.byte $00,$00,$00,$00
?enm120	.byte $7F,$3F,$1F,$0F,$FF

;**************************************************
	.sbttl "Initialize All Spheres"
;**************************************************
zero0		lda	difcty			;Multiply times nweb
		cmp	#03
		ifcs
			lda	#03				;Four waves
		endif
		sta	difmx3
		lda	#$80
		sta	sphrxl
		sta	corxl
		lda	#04
		sta	sphrxh
		sta	corxh
		lda	#$10
		sta	statdeg
		lda	#00
		sta	stbflg
		sta	shipadd
		sta	sphrad
		sta	sphrst
		sta	six
		lda	#$50
		sta	bonusa
		jsr	back2
		ldx	difmx3
		ldy	nhives,X
		lda	ztabidx,X
		tax	
		sty	websnum
		begin
			lda	zerotab,X
			sta	webss2,Y
			lda	#01
			sta	webssta,Y
			dex	
			dey	
		miend
		lda	#$FF
		sta	webssta+nsph-1
		rts	
		
nhives	.byte $05,$07,$08,$08
ztabidx	.byte $05,$0D,$16,$16
zerotab	.byte $00,$2A,$55,$80,$AA,$D5
		.byte $00,$20,$$40,$60,$80,$A0,$C0,$E0
		.byte $00,$1D,$39,$56,$72,$8F,$AB,$C8,$E4

;**********************************************
	.sbttl "Restore Statuses After ORR Code"
;**********************************************
back2		ldx	#$18
		lda	#00
		begin
			sta	sobjst,X
			dex
		miend
		lda	#00
		sta	sndcue,abs
		sta	sndcue+1,abs
		rts	
		
back		jsr	back2
		lda	#$FF
		sta	webssta+nsph-1			;Reset the restore indicator
		lda	difmx3
		ifeq
			lda	#04
			sta	difmx3
		endif
		tay						;Level to Y
		lda	backdata-1,Y
		sta	temp1
		ldx	websnum
		begin
			lda	websav+nsph,X
			ifne
				cmp	#$40
				ifcc
					lda	#01
					sta	webssta+nsph,X
					inc	temp1
					ldy	temp1
					lda	zerotab,Y
					sta	webss2+nsph,X
					lda	#00
					sta	websseg+nsph,X
					jsr	getrand
					ldy	difmx3
					and	backdata2,Y
					sta	webstim+nsph,X
					jsr	getrand
					and	#$7F
					sta	websper+nsph,X
				endif
			endif
			dex
		miend	
		lda	#00
		sta	sphrad
		lda	#$80
		sta	statdeg
		rts	
		
backdata	.byte $06,$0E,$0E
backdata2	.byte $00,$00,$00,$00,$03

;*********************************************
	.sbttl "Move Bug Position into xcomp"
;*********************************************
movcomp	lda	websxl+nsph,X
		sta	xcomp
		lda	websxh+nsph,X
		sta	xcomp+1
		lda	websyl+nsph,X
		sta	xcomp+2
		lda	websyh+nsph,X
		sta	xcomp+3
		rts	
		
;*********************************************
	.sbttl "Move xcomp into Bug X pos"
;*********************************************
compmov	lda	xcomp
		sta	websxl+nsph,X
		lda	xcomp+1
		sta	websxh+nsph,X
		lda	xcomp+2
		sta	websyl+nsph,X
		lda	xcomp+3
		sta	websyh+nsph,X
		rts	
		
;*********************************************
	.sbttl "Move and Display the Station"
;*********************************************
stdisp	lda	sphrst
		ifne					;Lower Rotation Post
			lda	frame
			and	#01
			clc	
			adc	cntrdeg
			sta	cntrdeg
			cmp	#$19
			ifcs
				dec	cntrdeg
			endif
			inc	sphrad
			ifeq
				dec	sphrad
			endif
			lda	#02
			clc	
			adc	statdeg
			sta	statdeg
			lda	statdeg
			jsr	stsup0
			sta	statyh
			lda	temp2
			sta	statyl
			lda	statdeg
			clc	
			adc	cntrdeg
			jsr	stsup0
			sta	sphryh
			lda	temp2
			sta	sphryl
			inc	sphrad			;Timer for bug starts
			lda	sphrad
			ifeq
				lda	#$FF
				sta	sphrad
			endif
			rts
		endif
		ldy	statdeg			;Upper rotation pole
		cpy	#$40
		ifcc
			tya	
			jsr	sin				;Make station move to resting point
			sta	temp2
			lda	#00
			ldx	#01
			begin
				asl	temp2
				rol	A
				dex	
			miend
			sta	temp2+1
			jsr	hvsup1			;Multiply Result to get 0 to 6 motion
			clc	
			adc	#03
			sta	sphryh
			lda	temp2
			sta	sphryl
			ldx	#03
			begin
				lda	sphrxl,X
				sta	statxl,X
				dex
			miend
			lda	#02
			clc	
			adc	statdeg
			sta	statdeg			;Move ship down to player in a half second
		else_pl
			inc	sphrad
			lda	sphrad
			cmp	#$10
			ifeq
				ldy	difmx3
				ldx	websnum			;Start dragonflies
				begin
					lda	#01
					sta	webssta+nsph,X		;Orbit motion
					lda	#00
					sta	websseg+nsph,X		;No extra bump as if just hit
					lda	webss2,X
					sta	webss2+nsph,X		;Every 20 degrees in spread
					jsr	getrand
					and	dropmask,Y			;Removable
					sta	webstim+nsph,X		;Drop from formation in first 4 rounds
					jsr	getrand
					and	#$7F
					sta	websper+nsph,X		;Drop from an arbitrary angle
					dex	
				miend
			else_mi
				cmp	#$1B
				ifcs
					lda	#00				;Remove spheres
					ldx	websnum
					begin
						sta	webssta,X
						dex
					miend
					lda	#01
					sta	sphrst
					lda	#$20
					sta	statdeg
					lda	#00
					sta	cntrdeg
				endif
				
			endif
		endif
		lda	sphrad
		cmp	#$10
		ifcc
			lda	#$AB
			sta	sphcol
			lda	#00
			sta	sphscl
			lda	#$FF
		else_ne
			sta	sphscl
			lda	#$FF
			sta	sphcol
			lda	#00
			ldx	#05
			begin
				asl	sphscl
				rol	A
				dex
			miend
			adc	#$FB
		endif
		sta	sphsch
		rts	

dropmask	.byte $03,$01,$01,$01

;******************************************
	.sbttl "Support for stdisp"
;******************************************
stsup0	jsr	sin
		sta	temp2
		lda	#00
		asl	temp2
		rol	A
		asl	temp2
		rol	A
		cmp	#02
		ifcs
			ora	#$FC
		endif
		sta	temp2+1
		lda	temp2
		clc	
		adc	#$80
		sta	temp2
		lda	temp2+1
		adc	#04
		rts
		
;**************************************************
	 .sbttl "Move and Display Sphere Hive Bombs"
;**************************************************	
hvdisp	ldx	webscur
		lda	webssta,X
		ifeq
			rts
		endif
		lda	webss2,X
		jsr	orsup1
		lda	sphscl
		sta	scalef
		lda	sphsch
		sta	scalef+1
		jsr	posvc2
		ldy	six
		lda	?mds115,Y
		ldx	sphcol
		cpx	#$FF
		ifeq
			ora	#08
		endif
		tax	
		lda	sphcol
		jsr	vgadd2
		ldx	six
		ldy	?mds120,X
		lda	cerstf,Y
		ldx	cerstf+1,Y
		jsr	vgadd2
		lda	sphrad
		cmp	#$10
		ifcc
			lda	#$40
			sta	temp4
			lda	#08
			sta	temp4+1
			lda	#00
			sta	hitpts				;No points for shooting spheres
			ldx	webscur
			jsr	hitwebs
			ldx	webscur
			lda	webssta,X
			ifmi
				lda	#snd_c3
				jsr	dosound
				ldx	webscur
			endif
			lda	#01					;Shots can't harm spheres
			sta	sobjst,X
		endif
		lda	webss2,X
		clc	
		adc	#02
		sta	webss2,X
		rts	
		
?mds115	.byte statsp,statsp+4,statsp,statsp+4,statsp,statsp+4
?mds120	.byte 0,4,2,2,4,0

;***********************************************
	.sbttl "Support for the Hive Display"
;***********************************************
hvsup0	ldx	#04
		begin				;Shift right five times
			lsr	A
			bit	temp2+1		;Sign extend
			ifmi
				ora	#$80
			endif
			ror	temp2
			dex
		miend
		sta	temp2+1
		rts
			
hvsup1	sta	temp3+1		;Multiply temp2 times 1.5
		lda	temp2
		clc	
		bit	temp3+1
		ifmi
			sec
		endif
		ror	temp3+1
		ror	A
		clc	
		adc	temp2
		sta	temp2
		lda	temp3+1
		adc	temp2+1
		sta	temp2+1
		rts
			
hvsup2	lda	temp2
		clc	
		adc	sphrxl,Y
		sta	xcomp,Y
		lda	temp2+1
		adc	sphrxh,Y
		sta	xcomp+1,Y
		rts
		
;***************************************************
	.sbttl "Display and move the Fish Bombs"
;***************************************************			
fsdisp	lda	#00
		sta	perm5
		lda	#$50
		sta	temp4
		lda	#$10
		sta	temp4+1
		ldx	webscur
		lda	webssta+nsph,X
		ifeq
			rts				;If it's dead, don't do anything
		endif
		cmp	#01
		ifeq
			jsr	orbit
		else
			inc	fishmys
			cmp	#02
			ifeq
				jsr	track
			else
				cmp	#03
				ifeq
					jsr	spiral
				else
					jsr	explode
				endif
			endif
		endif
		ldx	webscur
		lda	webssta+nsph,X
		ifeq
			rts
		endif
		lda	temp4
		ifne
			lda	webssta+nsph,X
			sta	perm1
			cmp	#$40
			ifcc
				tay
				lda	fishpts-1,Y
			else_ne
				lda	#00
			endif
			sta	hitpts
			txa	
			clc	
			adc	#nsph
			tax	
			lda	xcomp+2
			sec	
			sbc	#$C0				;Make sure can't hit it if behind player
			lda	xcomp+3
			sbc	#09
			ifcc
				lda	#01
				sta	maznum			;Removable
				jsr	hitwebs
				lda	#00
				sta	maznum			;Removable
				ldx	webscur
				lda	webssta+nsph,X
				ifmi
					lda	perm1
					cmp	#04
					ifcc
						cmp	#01
						ifeq
							lda	websseg+nsph,X
						 	ifeq
						 		lda	#$0C
								sta	websseg+nsph,X		;Faster speed for awhile
							endif
							lda	#snd_d3
							jsr	dosound			;Fishoid Hit sound
							ldx	webscur
							lda	#01
						else_pl
							sta	webss2+nsph,X
							lda	#snd_d4			;Destroy Fish
							jsr	dosound
							ldx	webscur
							lda	#$40
						endif
					endif
					sta	webssta+nsph,X
				endif
			endif
		endif
		lda	#$80
		sta	scalef
		lda	#$FE
		sta	scalef+1
		lda	perm5
		ifne
			lda	#00
			sta	temp2+1
			lda	perm5
			ldx	#03
			begin
				asl	A
				rol	temp2+1
				dex
			miend
			sta	temp2
			lda	#$80				;Generate growing scale
			clc	
			adc	temp2
			sta	scalef
			lda	#$F2
			adc	temp2+1
			sta	scalef+1
		endif
		jsr	posvc2
		ldx	webscur
		lda	webssta+nsph,X
		cmp	#$40
		ifcs
			jmp	fsexpl
		endif
		ldx	webscur
		lda	webssta+nsph,X
		cmp	#01
		ifeq
			ldy	websseg+nsph,X
			ifne
				lda	#02
			endif
		endif
		sta	perm4
		lda	bugim
		and	#08
		lsr	A
		ora	#statsp
		tax	
		sta	perm1					;perm1 is the xflip and sparkle
		ldy	perm4
		lda	fishcol1-1,Y
		jsr	vgadd2
		lda	bugim
		sta	temp1
		and	#07
		asl	A
		tay	
		lda	cerpup,Y
		ldx	cerpup+1,Y
		jsr	vgadd2
		ldy	perm4
		lda	fishcol2-1,Y
		ldx	perm1
		jsr	vgadd2
		lda	bugim
		and	#$30
		lsr	A
		lsr	A
		lsr	A
		sta	temp1
		lda	bugim
		and	#07
		asl	A
		asl	A
		asl	A
		adc	temp1
		tay						;Matrix of wing positions
		lda	cerwng,Y
		ldx	cerwng+1,Y
		jmp	vgadd2
		rts
		
fishpts	.byte $01,$10,$10
fishcol1	.byte $F6,$F7,$F7
fishcol2	.byte $F3,$8B,$8B

;******************************************************
	.sbttl "Rotate the bugs around the Sphere Center"
;******************************************************
orbit		lda	webss2+nsph,X
		clc	
		adc	#$40					;Bug points perpendicular to direction of motion
		jsr	fssup0
		lda	webss2+nsph,X
		jsr	orsup1
		ldx	webscur
		jsr	compmov				;Put position in websxylx
		lda	websseg+nsph,X
		ifne
			inc	fishmys
			dec	websseg+nsph,X
			lda	#04
		else_ne
			lda	#01
		endif
		sta	temp1
		clc	
		adc	webss2+nsph,X
		sta	webss2+nsph,X			;Rotation
		lda	shipst
		ifpl
			ifne
				ifvs
					lda	webstim+nsph,X		;Ready to releast a bug?
					ifmi					;yes
dropfish					lda	#02
						sta	webssta+nsph,X
						lda	webss2+nsph,X
						adc	#$3F				;Correct for look/move direction
						sta	webss2+nsph,X
						rts	
					else					;nope, decrement it
						dec	webstim+nsph,X
					endif
				endif
				lda	webstim+nsph,X
				ifmi
					lda	webss2+nsph,X
					and	#$7F
					cmp	websper+nsph,X		;Check for degree release
					bcs	dropfish
				endif
				lda	websseg+nsph,X
				ifeq
					lda	temp1
					cmp	#04
					beq	dropfish
				endif
			endif
		endif
		rts
			
orbit2	ldy	incdif
		lda	fishmys
		cmp	orbval,Y
		ifcc
			ldx	websnum
			begin
				lda	webssta+nsph,X
				cmp	#01
				ifeq
					lda	websseg+nsph,X
					ifeq
						jmp	dropfish
					endif
				endif
				dex	
		      miend
		endif
		rts
			
orbval	.byte $01,$01,$02,$02,$03

;*******************************************
	.sbttl "Support for Orbit"
;*******************************************
orsup0	sta	temp2
		sta	temp3
		lda	#00
		asl	temp2
		rol	A
		asl	temp2
		rol	A
		bit	temp3
		ifmi
			ora	#$FC
		endif
		sta	temp2+1
		rts
		
;*******************************************
	.sbttl "Support for Orbit"
;*******************************************	
orsup1	sta	perm1
		jsr	cos
		jsr	orsup0			;Multiply by radius quickly
		jsr	hvsup1
		ldy	#00
		jsr	hvsup2			;Store away to xcomp
		lda	perm1
		jsr	sin
		jsr	orsup0
		ldy	#02
		jsr	hvsup2
		rts	
		
;********************************************
	.sbttl "Dragonfly Tracks the Player"
;********************************************
rot0	=	6
rot1	=	6
rot2	=	12
rot3	=	18

track		jsr	movcomp
		lda	difmx3
		and	#03
		tay	
		lda	webss2+nsph,X
		cmp	#$C0
		ifcs
			lda	fishrot,Y			;If pointing up, turn down by fastest method
		else_ne				;Turn to right
			cmp	#$80
			ifcs
				lda	frotl,Y			;Turn to left
			else_ne
				lda	xcomp				;Otherwise turn to chase player
				sec	
				sbc	corxl
				lda	xcomp+1
				sbc	corxh				;Randomness added to motion
				ifcs					;Bug is to right of ship
					lda	webss2+nsph,X
					cmp	frotr,Y
					ifcc
						lda	fishrot,Y
					else_pl
						lda	#$7F
						sta	webss2+nsph,X		;Persue as nearly a horizontal course as possible
						lda	#00
					endif
				else_pl
					lda	webss2+nsph,X
					cmp	fishrot,Y
					ifcc
						lda	#00
						sta	webss2+nsph,X
					else_pl
						lda	frotl,Y
					endif
				endif
			endif
		endif
		sta	temp1				;Suggested direction change
		lda	xcomp+2
		sec	
		sbc	#$C0
		lda	xcomp+3
		sbc	#08
		ifcs					;Bottom of screen
			lda	webss2+nsph,X
			adc	#$3F
			ifmi					;Turn to point straight down
				lda	frotl,Y
			else_ne
				cmp	frotr,Y
				ifcs
					lda	#00
				else_eq
					lda	fishrot,Y
				endif
			endif
			sta	temp1
		endif
		lda	temp1
		clc
		adc	webss2+nsph,X
		sta	webss2+nsph,X
		jmp	fsdegr
		
frotr		.byte $7A,$7A,$74,$6E
frotl		.byte $FA,$FA,$F4,$EE
fishrot	.byte $06,$06,$0C,$12

;********************************************************
	.sbttl "Control the Exploding Dragonfly"
;********************************************************
explode	lda	webssta+nsph,X
		clc	
		adc	#01
		sta	webssta+nsph,X
		ifmi
			lda	#00
			sta	webssta+nsph,X
			rts
		endif
		lda	webssta+nsph,X
		cmp	#$60
		ifcc
			and	#$3F
			cmp	#$11
			ifcs
				lda	#$60
				sbc	webssta+nsph,X
			endif
			tax	
			lda	?fex100,X
			sta	perm5
		endif
		ldx	webscur
		jsr	movcomp
		lda	#00
		sta	perm4
		sta	temp4
		rts
			
?fex100	.byte $00,$94,$B4,$C6,$D4,$DE,$E6,$ED
		.byte $E6,$DB,$CB,$D9,$E3,$ED,$F5,$FB,$FF
		
;********************************************************
	.sbttl "Dragonfly draws a constricting spiral"
;********************************************************
spiral	jsr	movcomp
		lda	webss2+nsph,X
		sta	perm1					;Angle of dragonfly
		lda	websseg+nsph,X
		ifpl						;Rotation Number
			lda	webstim+nsph,X			;Non zero if in a straight horizontal
			ifne
				dec	webstim+nsph,X
				ifeq
					inc	perm1					;Finished straight section
					ifpl
						dec	websseg+nsph,X			;Rotation complete
					endif
				endif
			endif
		endif
		lda	difmx3
		and	#03
		tay	
		lda	webss2+nsph,X
		cmp	#$C0
		ifeq
			lda	xcomp+2
			sbc	sphheight,Y				;Begin backturn at this height
			lda	xcomp+3
			sbc	sphturn,Y
			ifcc
				inc	perm1
				jsr	fishsnd
			endif
		endif
		lda	webss2+nsph,X
		cmp	#$40						;Begin upturn at height dependent on time
		ifeq
			sec
			lda	websseg+nsph,X
			ifpl
				lda	xcomp+2
				sbc	fturn1,Y
			else
				lda	xcomp+2
				sbc	fturn2,Y
			endif
			lda	xcomp+3
			sbc	#09
			ifcs
				inc	perm1					;Begin turn
				jsr	fishsnd
			endif
		endif
		lda	perm1
		and	#$3F
		ifne						;In process of turning
			cmp	ftrnlen,Y
			lda	perm1
			ifcs
				clc
				adc	fishrot,Y				;cs
				and	#$C0					;Stop at 0,40,80,C0
				sta	perm1
				and	#$40
				ifeq
					lda	websseg+nsph,X			;Need to get new horizontal length
					ifpl
						bit	perm1
						ifmi
							clc
							adc	#04
						endif
						sta	temp1
						lda	difmx3
						asl	A
						asl	A
						asl	A
						adc	temp1
						tay
						lda	fvar,Y
						sta	webstim+nsph,X
					endif
				endif
			else_ne
				adc	fishrot,Y
				sta	perm1
			endif
		endif
		lda	perm1
		sta	webss2+nsph,X
		lda	websper+nsph,X				;Incorporate reflection
		ifeq
			lda	perm1
		else
			lda	#$80
			sec
			sbc	perm1
		endif
		jmp	fsdegr
		
ftrnlen	.byte $40-rot0,$40-rot1,$40-rot2,$40-rot3
sphheight	.byte $00,$40,$00,$00
sphturn	.byte $08,$09,$09,$09
fturn1	.byte $C0,$90,$90,$90
fturn2	.byte $60,$38,$38,$38
fvar		.byte $01,$02,$03,$04,$01,$02,$04,$06,$01,$02,$03,$04,$01,$02,$03,$06
		.byte $01,$01,$02,$03,$01,$01,$03,$04,$01,$01,$01,$02,$01,$01,$01,$03
unknown1	.byte $08,$10,$20,$40
unknown2	.byte $08,$10,$30,$60


fishsnd	lda	sndcue+1,abs
		ifeq
			lda	#$18
			sta	sndcue+1,abs
			lda	#snd_d5
			jmp	dosound
		endif
		rts	
		
;*****************************************************
	.sbttl "Generates Velocity from Fish Direction"
;*****************************************************
fsdegr	sta	perm1
		jsr	fssup0
		lda	perm1
		jsr	cos				;New X Velocity
		sta	temp1
		ldy	difmx3
		ldx	webscur
		lda	webssta+nsph,X
		cmp	#03
		ifne
			ldx	fsd60,Y
			bpl	?fsd5
		endif
		ldx	fsd65,Y
?fsd5		jsr	fssup1
		sta	bugxv
		lda	perm1
		jsr	sin
		sta	temp1
		ldy	difmx3
		ldx	webscur
		lda	webssta+nsph,X
		cmp	#02
		ifeq
			lda	xcomp+3
			cmp	#07
			bcs	?fsd10
			ldx	fsd70,Y
		else_ne
?fsd10		ldx	fsd75,Y			;Move slow when low down
		endif
		jsr	fssup1
		sta	bugyv
		ldx	webscur
		lda	bugxv
		ifmi
			dec	xcomp+1
		endif
		clc	
		adc	xcomp
		sta	xcomp
		ifcs
			inc	xcomp+1
		endif
		lda	xcomp+1
		ifmi
			lda	#00
			sta	xcomp
			sta	xcomp+1
		endif
		cmp	#02
		ifcc
			lda	websper+nsph,X
			eor	webss2+nsph,X
			ifne
				jsr	fsrefl
			endif
		endif
		lda	xcomp+1
		cmp	#09
		ifcs
			lda	#08
			sta	xcomp+1
			lda	#$FF
			sta	xcomp
		endif
		cmp	#07
		ifcs
			lda	websper+nsph,X
			eor	webss2+nsph,X
			ifeq
				jsr	fsrefl
			endif
		endif
		lda	bugyv
		ifmi
			dec	xcomp+3
		endif
		clc	
		adc	xcomp+2
		sta	xcomp+2
		lda	xcomp+3
		adc	#00
		sta	xcomp+3
		lda	webssta+nsph,X
		cmp	#03
		ifne
			lda	xcomp+2
			sec	
			sbc	#$40
			lda	xcomp+3
			sbc	#09
			ifcs
				lda	#$40
				sta	webss2+nsph,X			;Spiral dragonfly at player level
				lda	#03
				sta	webssta+nsph,X
				lda	#03					;Removable
				sta	websseg+nsph,X			;Number of circles
				lda	#00
				sta	webstim+nsph,X			;Going down, not horizontally
				lda	xcomp
				sbc	#$80
				lda	xcomp+1
				sbc	#04
				ifcc
					lda	#$80
				else_mi
					lda	#00
				endif
				sta	websper+nsph,X			;Reflect direction
			endif
		endif
		jsr	compmov				;Move variables back
		rts	
		
fsd60		.byte $01,$01,$00,$00,$02
fsd65		.byte $01,$01,$00,$00,$03
fsd70		.byte $02,$01,$01,$01,$03
fsd75		.byte $03,$02,$01,$01,$03
fcoreg	.byte $02,$01,$01,$00,$08,$09,$09,$0A
		.byte $0B,$0C,$0C,$0D,$05,$04,$04,$03

;********************************************
	.sbttl "Support for fsdegr"
;********************************************
fssup0	lsr	A
		lsr	A
		lsr	A
		lsr	A
		tay	
		lda	frame
		and	#03
		asl	A
		asl	A
		asl	A
		asl	A
		ora	fcoreg,Y
		sta	bugim
		rts
			
fssup1	lda	temp1
fssupn2	cpx	#00
		ifne
			begin						;Removable, div2x
				cmp	#$80
				ror	A
				dex
			miend
			rts
		endif
		sta	temp1
		cmp	#$80
		ror	A						;Multiply times 5
		lsr	A
		sta	temp1+1
		lsr	A
		clc	
		adc	temp1+1					;Multiply times 75
		bit	temp1
		ifmi
			clc
			adc	#$40
		endif
		rts	
		
;******************************************************
	.sbttl "Reflect the Fish Bomb at Edge of Screen"
;******************************************************
fsrefl	lda	webssta+nsph,X
		cmp	#03
		ifeq
			lda	websseg+nsph,X
			ifmi
				lda	bugim
				eor	#08
				sta	bugim					;Reverse direction of screen image
				lda	webss2+nsph,X
				and	#$80
				eor	#$80
				sta	webss2+nsph,X
			endif
		endif
		rts
		
;**************************************************
	.sbttl "Display the exploding Fish"
;**************************************************		
fsexpl	lda	#00
		sta	perm2
		lda	webss2+nsph,X
		sta	perm4
		lda	webssta+nsph,X
		and	#$3F
		lsr	A				;Get the explosion frame
		sta	perm1
		sta	perm1+1
		lsr	A
		ror	perm2				;Minus if an odd frame, zero if even
		asl	perm1+1
		asl	perm1+1
		txa	
		and	#04				;Xflip of sequence
		ora	#statsp
		sta	temp2
		ldy	#00
		ldx	perm4				;Choreography type
		lda	perm5
		ifne
			jsr	getrand
			and	#$7F
			ora	#$20				;Get random color and random restricted intensity
			sta	(vglist,Y)			;Eye color
			iny
			lda	temp2
			sta	(vglist,Y)
			iny	
			lda	cerbng
			sta	(vglist,Y)
			iny
			lda	cerbng+1
			sta	(vglist,Y)
			iny					;Draw the start at explosion center
		endif
		lda	perm1
		lsr	A
		eor	#$0F
		ifeq
			lda	#01
		endif					;Get non-zero intensity
		asl	A
		asl	A
		asl	A
		asl	A
		ora	#$0A				;Body color
		sta	(vglist,Y)			;Fading intensity
		iny	
		lda	temp2
		sta	(vglist,Y)
		iny	
		clc	
		lda	xcomp+2
		adc	#$80
		pha					;Get scale for explosion
		lda	xcomp+3
		adc	#$FE
		sta	temp1
		clc	
		ifmi
			sec
		endif					;Good place for routineing, removeable
		pla	
		ror	temp1
		ror	A
		lsr	A
		tax	
		lda	fullog,X
		sta	(vglist,Y)			;Linear Scale
		iny	
		lda	#$76
		sec	
		sbc	temp1
		sta	(vglist,Y)
		iny					;Binary scale
		lda	perm1
		and	#$FE
		asl	A
		tax					;Four bytes of data every other frame
		bit	perm2
		ifpl
			begin
				lda	?dfh100,X
				sta	(vglist,Y)
				iny	
				inx	
				tya	
				and	#03
			eqend
		else_eq
			begin
				lda	?dfh100,X			;Cc from loop end or getting here
				clc	
				adc	?dfh100+4,X
				sta	temp1
				lda	?dfh100+1,X
				adc	?dfh100+5,X
				and	#$DF
				cmp	#$10
				ifcs
					ora	#$20				;Sign extend to $3f in high byte
				endif
				lsr	A
				ror	temp1
				pha	
				lda	temp1
				sta	(vglist,Y)
				iny					;Get averaged vector	
				pla	
				sta	(vglist,Y)
				iny	
				inx	
				inx	
				tya
				and	#02
			eqend					;Do it twice
		endif
		lda	perm1
		tax	
		and	#03
		asl	A
		sta	perm3				;Rotation sequence of four
		lda	?dfh125,X
		sta	perm3+1			;Rotation sequence of six
		ldx	#00
		beq	?dfh20			;No vector first time around
		begin
			lda	#00	
			sta	temp2
			sta	temp2+1
			stx	temp1
			ldx	perm4+1
			lda	perm4
			ifmi
				lda	#00
				sec	
				sbc	?dfh115,X
			else
				lda	?dfh115,X
			endif
			ldx	temp1
			asl	A
			rol	temp2
			asl	A
			rol	temp2
			sta	(vglist,Y)
			iny	
			lda	temp2
			cmp	#02
			ifcs
				ora	#$1C
			endif
			sta	(vglist,Y)
			iny	
			stx	temp1
			ldx	perm4+1
			lda	?dfh110,X
			ldx	temp1
			asl	A
			rol	temp2+1
			asl	A
			rol	temp2+1
			sta	(vglist,Y)
			iny	
			lda	temp2+1
			cmp	#02
			ifcs
				ora	#$1C				
			endif
			sta	(vglist,Y)
			iny	
?dfh20		lda	?dfh120,X
			clc	
			ifmi
				and	#$7F
				adc	perm3
			else_ne
				adc	perm3+1				;Add in rotation cycle
			endif
			stx	temp1
			tax	
			lda	cerbng,X
			sta	(vglist,Y)
			iny	
			lda	cerbng+1,X
			sta	(vglist,Y)
			iny	
			ldx	temp1
			inx	
			lda	?dfh105,X
			sta	perm4
			and	#$3F
			clc	
			adc	perm1+1
			sta	perm4+1
			cpx	#08
		csend
		tya	
		clc	
		adc	vglist
		sta	vglist
		ifcs
			inc	vglist+1
		endif							;Removeable
		rts	

?dfh100	.byte $00,$00,$00,$00,$F4,$1F,$C1,$1F,$E8,$1F,$84,$1F,$DB,$1F,$49,$1F
		.byte $CE,$1F,$10,$1F,$C1,$1F,$D9,$1E,$B4,$1F,$A4,$1E,$A6,$1F,$71,$1E
		.byte $98,$1F,$40,$1E,$8A,$1F,$11,$1E,$7C,$1F,$E4,$1D,$6D,$1F,$B9,$1D
		.byte $5E,$1F,$90,$1D,$4F,$1F,$69,$1D,$40,$1F,$44,$1D,$30,$1F,$21,$1D
		.byte $20,$1F,$00,$1D
		
?dfh105	.byte $00,$00,$01,$02,$03,$82,$81,$80
		
?dfh110	.byte $00,$00,$00,$00

trtbl		.byte $FC,$04,$05,$06,$F8,$08,$0A,$0C,$F4,$0C,$0F,$12,$F0,$10,$14,$17
		.byte $EC,$14,$19,$1D,$E9,$17,$1D,$22,$E5,$1B,$22,$27,$E2,$1E,$26,$2C
		.byte $DE,$22,$2B,$31,$DB,$25,$2F,$36,$D7,$29,$34,$3B,$D4,$2C,$38,$3F
		.byte $D1,$2F,$3C,$44,$CE,$32,$40,$48,$CB,$35,$44,$4C,$C8,$38,$48,$50
		.byte $C5,$3B,$4C,$54,$C2,$3E,$50,$58,$BF,$41,$54,$5C,$BC,$44,$58,$5F
		.byte $B9,$47,$5C,$63,$B7,$49,$5F,$66,$B4,$4C,$63,$69,$B2,$4E,$66,$6C
		.byte $AF,$51,$6A,$6F,$AD,$53,$6D,$72,$AA,$56,$71,$75,$A8,$58,$74,$77
		.byte $A6,$5A,$77,$7A,$A4,$5C,$7A,$7C,$A2,$5E,$7D,$7E
		
?dfh115	.byte $00,$00,$00,$00,$03,$03,$0F,$00,$06,$06,$1C,$00,$09,$09,$29,$00
		.byte $0B,$0C,$34,$00,$0E,$0F,$3F,$00,$10,$12,$48,$00,$12,$15,$51,$00
		.byte $14,$18,$58,$00,$16,$1B,$5F,$00,$18,$1E,$64,$00,$1A,$21,$69,$00
		.byte $1B,$24,$6C,$00,$1D,$27,$6F,$00,$1E,$2A,$70,$00,$1F,$2D,$71,$00
		.byte $20,$30,$70,$00,$21,$33,$6F,$00,$22,$36,$6C,$00,$23,$39,$69,$00
		.byte $23,$3C,$64,$00,$24,$3F,$5F,$00,$24,$42,$58,$00,$24,$45,$51,$00
		.byte $24,$48,$48,$00,$24,$4B,$3F,$00,$24,$4E,$34,$00,$24,$51,$29,$00
		.byte $23,$54,$1C,$00,$23,$57,$0F,$00,$22,$5A,$00,$00,$21,$5D,$F1,$00
		
?dfh120	.byte $9A,$02,$0E,$0E,$9A,$02,$9A,$0E
?dfh125	.byte $00,$02,$04,$06,$08,$0A,$00,$02,$04,$06,$08,$0A,$00,$02,$04,$06
		.byte $08,$0A,$00,$02,$04,$06,$08,$0A,$00,$02,$04,$06,$08,$0A,$00,$02

;**************************************************************
	.title "Reactor Routines"
	.sbttl "Reactor Color Control"
;**************************************************************
;* If reactor status is + (safe) then color pulses just       *
;* yellow. If reactor status is - (ready to blow) then color  *
;* is red/yellow pulses.                                      *
;*                                                            *
;* Uses: temp1                                                *
;**************************************************************
react	lda	frame				;Continue to move intensity
		asl	A
		asl	A
		asl	A
		ora	#$20				;Min brightness
		and	#$F0
		sta	reintn			;Save intensity
		bit	objst+zreactor		;Status?
		ifmi					;Time countdown
			lda	frame
			and	#07
			ifeq					;Rod pull out time
				lda	rodstat			;Need to move out rods?
				cmp	#06
				ifcc					;Under 3, will move
					inc	rodstat			;Next pic
					inc	rodstat			;Next pic
				endif
			endif
			bit	gamest			;Exit??
			ifvc					;Nope, Ok to count
				lda	manstat			;Skip if dead or dying
				ifpl
					ifne
						lda	gamest
						and	#$10				;Count hold??
						ifeq					;Nope
							lda	sndcue+2,abs
							ifeq					;Time to count down
								lda	#$22
								sta	sndcue+2,abs
								lda	#snd_d2			;Normal sound
								jsr	dosound			;Sound this thing off
							endif
							lda	#07
							clc
							adc	incdif
							sta	temp1				;Amount to subtract from reactor LSB each frame
							lda	retime+1
							sec
							sbc	temp1
							sta	retime+1			;Reactor time LSB
							ifcc
								sed
								lda	retime
								sec
								sbc	#01
								sta	retime			;Reactor time
								cld
								ifeq				;Time to blow?
									lda	#00
									sta	retime+1		;Reactor time LSB
									sta	nxtexp			;For in maze explosion
									lda	#$80
									sta	manstat
									sta	tspark			;Blow up so turn off drawing
									sta	nodraw			;Not sparkle draw either
									lda	#$20
									sta	objst+zreactor		;Boom
									;Mark - I need a flag here that says don't draw maze and things
								endif
							endif
						endif
					endif
				endif
			endif
		else					;Stay yellow if OK
			lda	#00
			sta	rodstat			;Rods stay in if safe
		endif
		lda	vglist
		pha					;Save current vglist	
		lda	vglist+1
		pha	
		lda	#(retbuf+$0A)/$100&$ff
		sta	vglist+1
		lda	#(retbuf+$0A)&$ff
		sta	vglist
		lda	#$A0				;Counting or dead?
		bit	objst+zreactor
		ifeq
			laljsr(char_space)
			lxhjsr(char_space)
			jsr	vgadd2			;space
			laljsr(char_space)
			lxhjsr(char_space)				
			jsr	vgadd2			;space
			laljsr(char_o)
			lxhjsr(char_o)
			jsr	vgadd2			;Letter O
			laljsr(char_k)
			lxhjsr(char_k)
			jsr	vgadd2			;Letter K
			laljsr(char_space)
			lxhjsr(char_space)
			jsr	vgadd2			;space
		else
			lda	retime
			sta	temp1
			lda	#00
			sta	temp1+1
			lda	#temp1
			sec	
			ldy	#02
			jsr	digits
		endif
		pla	
		sta	vglist+1
		pla	
		sta	vglist			;restore original vglist
	;Fall Through to next routine
;****************************************************
	.sbttl "Reactor Draw Routine"
;****************************************************
;* Will draw the reactor in the proper color if it  *
;* is visible on the screen. Uses routine 'locate'  *
;* to place the object.                             *
;****************************************************
reac2		lda	tspark			;Don't draw reactor while beaming
		bne	?rac1
		ldx	#zreactor			;Reactor number
		jsr	locate			;Put it on screen if ready
		tya	
		ifpl					;Ready
?rac1			jmp	endre1
		endif
		lda	objst+zreactor
		ifpl
			lda	sndcue+1,abs
			ifeq
				lda	#$40
				sta	sndcue+1,abs
				lda	#snd_b1d				;Hmmmmmmmmmmmm
				jsr	dosound
			endif
		endif
		lda	#$F5				;Purple reactor??
		bit	objst+zreactor
		ifmi
			lda	#$C6				;Yellow if set off
		endif
		ldx	#body7+xflip		;Stat, page select, flip
		jsr	vgadd2
		lda	#01
		ldy	#00
		jsr	vgscal			;Size for this object
		laljsr(body)
		lxhjsr(body)				;Use robot body for reactor
		jsr	vgadd2
		lda	#$0F
		bit	objst+zreactor
		ifmi
			lda	#$0A
		endif
		ora	reintn			;Add intensity
		ldx	#rods7+xflip		;Stat, page select, xflip on
		jsr	vgadd2			;Color for rods and button
		ldy	rodstat			;Rod status
		lda	rods,Y
		ldx	rods+1,Y
		jsr	vgadd2			;Add rods and button
		lda	#$0A				;Funny red here
		ora	reintn			;Color and intensity
		ldx	#$60+sparkle+xflip	;Stat, sparkle and flip
		bit	objst+zreactor		;Counting??
		ifmi
			ldx	#$60+xflip			;Turn on sparkle
		endif
		jsr	vgadd2			;Add 2 bytes to list
		ldy	retime			;Ball grows with time
		ifeq
			ldy	#$B0				;If not couting, hold at small
		endif
		lda	#02
		jsr	vgscal
		laljsr(sparkb)
		lxhjsr(sparkb)
		jsr	vgadd2
		lda	#blank
		ldx	#$60
		jsr	vgadd2			;Restore normal stat
		lda	#$A3
		bit	gamest			;Game over??? (Attract?)
		ifpl					;yep
			lda	#00
		endif
		ifne
			bit	objst+zreactor
			ifmi
				lda	dif4mz			;wave 0, reactor set off?
				ifeq
					lda	#$D8
					tax
					jsr	vgvtr5
					ldx	#mgetout
					stx	temp4
					jsr	msgnop			;Get out message
				endif
			endif
		endif
;****************************************************
	.sbttl "Blow up from Within Check and Call"
;****************************************************
endre1	lda	objst+zreactor		;Make sure reactor is not alive
		and	#$BF				;Drop screen bit
		cmp	#01
		ifne
			lda	retime			;And time=0
			ifeq
				lda	manstat			;Man dying??
				and	mzgame			;And still in maze
				ifmi
					jsr	inblow			;Blow this thing!
				endif
			endif
		endif
		rts
		
;************************************************
	.sbttl "Sparks Routine"
;************************************************
sparks	lda	#sparkb&$ff
		sta	vglist
		lda	#sparkb/$100
		sta	vglist+1				;Set up output sparkler
		ldy	#nmsparks-1				;Do all sparks
		sty	temp3+1
		begin
			ldy	temp3+1				;Sparks left to do
			lda	frame
			clc	
			asl	A
			adc	ofsets,Y				;Group offsets, so that sparklets start at different time
			and	#$0E					;Longest length possible
			sta	temp1
			ifeq						;New vector
				jsr	getrand
				sta	sparkangle,Y
			endif
			lda	sparkangle,Y
			jsr	sin					;Now use angle, get X and Y components
			jsr	multiply
			sta	temp6					;Save Y
			ldy	temp3+1				;Recall Y
			lda	sparkangle,Y			;Recall angle
			jsr	cos
			jsr	multiply
			sta	temp5				
			;Now prepare to write vector
			ldx	temp6
			ldy	#$20					;Brightness
			jsr	vgvtr
			;Now reverse the vector
			lda	#00
			sec	
			sbc	temp6
			tax	
			lda	#00
			sec	
			sbc	temp5
			ldy	#$20					;Brightness
			jsr	vgvtr
			dec	temp3+1
		miend
		jsr	vgrtsl				;Add an RTSL to the buffer
		rts
			
ofsets	.byte 0d,8d,16d,24d,36d,48d

;****************************************
	.sbttl "Fire Ball"
;****************************************
;* Moves and contros wandering fireballs*
;* in the maze, as well as laser cannon *
;* shots.                               *
;*                                      *
;* Inputs: Object X,Y, Status, VelX & Y *
;*                                      *
;* Output: Update position and status.  *
;*                                      *
;* Uses:   (see movthg)                 *
;*         +2 additional bytes stack    *
;****************************************
fire		lda	tspark				;Beaming!!
		ifne
			jmp	drtran				;Skip all drawing routines except man and transporter
		endif						
		ldx	#zfire+nmfire+nmlsht-1		;Number of fireballs and cannon shots
		stx	temp9
		begin
			cpx	#zfire+nmfire
			ifcc						;Fireball
				ldy	#fircol+$f0				;Guess normal
				lda	objst,X				;Launched
				and	#$10
				ifne						;yep
					ldy	#$FB					;They are red
				endif
				tya						;Color in A
				ldx	#shtex7
			else						;Laser Cannon Shots
				lda	#$F3
				ldx	#lshot7
			endif
			jsr	vgadd2			;Add color
			ldx	temp9
			lda	objst,X
			ifne
				ifmi					;Exploding??
					lda	objst,X
					clc	
					adc	#08				;Explosion speed
					sta	objst,X
					ifpl
						lda	#00
						sta	objst,X			;Kill this one and kill rest on this one
						beq	?fir10
					endif
				else					;Don't move explosions
					cpx	#zfire+nmfire		;Is it a fireball?
					ifcc
						jsr	movtng			;Yes, move it very slow
					else
						jsr	movtn2			;Laser shots are uneffected
					endif
				endif
				ldx	temp9
				jsr	bounce
				jsr	bump
				jsr	locate			;Place it!
				tya					;Did it place?
				ifmi					;yep
					ldx	temp9
					lda	objst,X			;Explode?
					ifpl					;no!
						cpx	#zfire+nmfire
						ifcs
							lda	#00				;Scale to full size if laser cannon shot
							ldx	#$72
							jsr	vgadd2
							lda	temp9
							asl	A
							clc
							adc	frame
							asl	A
							and	#$0E
							tay
							lda	newshot,Y
							ldx	newshot+1,Y
						else
							laljsr(sparkb)
							lxhjsr(sparkb)			;Add JSR to sparkbuffer
						endif
					else
						jsr	splash
					endif
					jsr	vgadd2
				endif
			endif
?fir10		dec	temp9
			ldx	temp9
			cpx	#zfire			;Last one
		ccend
;****************** Fall Through *********************
	.sbttl "Laser Cannon in Maze"
;*****************************************************
canndr	ldx	#zcann+nmcann-1
		stx	temp9
		begin
			lda	objst,X				;Make sure cannon is on
			ifne
				jsr	cannon
			endif
			dec	temp9
			ldx	temp9
			cpx	#zcann
		ccend
;****************** Fall Through *********************
	.sbttl "Discs in Maze"
;*****************************************************
discs		lda	#$F5			;They are all purple if reactor is subcritical
		bit	objst+zreactor
		ifmi
			lda	#$F6			;And yellow if it is supercritical
		endif
		ldx	#shld7		;Stat and page select
		jsr	vgadd2
		ldx	#zdisc+nmdisc-1
		stx	temp9
		begin
			lda	objxh,X		;Hit??
			ifne
				ifpl
					jsr	locate
					tya
					ifmi
						lda	#00
						ldx	#$75
						jsr	vgadd2			;Scale down
						laljsr(shield)
						lxhjsr(shield)
						jsr	vgadd2
						lda	#00
						ldx	#$73				;Small 2
						jsr	vgadd2
						lda	#08
						ldx	#-12				;Move over a bit
						jsr	vgvtr5
						lda	vgmsga+6			;Get JSRL into 2
						ldx	vgmsga+7
						jsr	vgadd2
					endif
				endif
			endif
			dec	temp9
			ldx	temp9
			cpx	#zdisc
		ccend
;********** Fall Through ********************
	.sbttl "Slow Down Clock"
;********************************************
;* Slow down clock - Concentric boxes       *
;* growing inside a larger one.             *
;********************************************
drclock	lda	objst+zstuf
		ifeq
?drc5			jmp	drboot
		endif
		lda	objst+zreactor		;Moves fast before touch
		ifpl
?drc10		dec	sldnfr
		else
			lda	frame
			and	#$0F				;Move slow
			beq	?drc10
		endif
		ldx	#zstuf			;Now use sldnfr to generate a picture
		jsr	locate
		tya	
		bpl	?drc5
		lda	#$E6
		ldy	objst+zstuf			;Set off?
		cpy	#02				;Yes if 2
		ifeq
			lda	#$FB				;Yellow becomes red!!
		endif
		ldx	#clock7
		jsr	vgadd2			;Same color as exploding fish
		lda	#$60
		ldx	#$71
		jsr	vgadd2
		lda	gclock
		ldx	gclock+1
		jsr	vgadd2			;Draw clock body
		lda	#$F7
		ldx	#clock7
		jsr	vgadd2			;White
		lda	gclock+2
		ldx	gclock+3
		jsr	vgadd2			;For the tick marks on the clock
		lda	#$60
		ldx	#$75
		jsr	vgadd2			;Now do hands
		lda	#$FB
		ldy	objst+zstuf			;Set off??
		cpy	#02
		ifeq					;yes
			lda	#$E6				;Set the red to yellow!!! Why??????
								;Because it's stupid and Mark said so!!!
		endif					;Unconnected events
		ldx	#clock7			;Hands color
		jsr	vgadd2
		lda	sldnfr
		pha	
		pha	
		asl	A
		asl	A
		asl	A
		pha	
		jsr	sin
		sta	perm2				;Y + coordinate of minute hand
		jsr	neg
		sta	perm2+1			;Y - coordinate of minute hand
		pla	
		jsr	cos
		pha					;X + coordinate of minute hand
		jsr	neg
		sta	perm1+1			;X - coordinate of minute hand
		pla					;X coordinate
		ldx	perm2				;Y coordinate
		ldy	#$20				;Intensity
		jsr	vgvtr
		lda	perm1+1			;X coordinate back
		ldx	perm2+1			;Y coordinate back
		ldy	#00				;not visible
		jsr	vgvtr
		lda	#$60
		ldx	#$76
		jsr	vgadd2			;Hour hand is half as large (yup! cheap way!)
		pla	
		jsr	sin
		sta	perm2				;Y + coordinate
		pla	
		jsr	cos				;X coordinate
		ldx	perm2				;Y coordinate
		ldy	#$20
		jsr	vgvtr
;********** Fall Through ***********************
	.sbttl "Magic Boots"
;***********************************************
drboot	lda	objst+zstuf+1
		cmp	#01
		ifeq					;1 means they are out there but not picked up
			ldx	#zstuf+1
			jsr	locate
			tya
			ifmi
				lda	#$AB
				ldx	#boot7
				jsr	vgadd2			;Put on your red shoes and dance the blues....
				lda	#00
				ldx	#$71
				jsr	vgadd2			;Set scale
				lda	gboot
				ldx	gboot+1
				jsr	vgadd2			;Draw them
			endif
		endif
;********** Fall Through ***********************
	.sbttl "Hand that turns off the reactor"
;***********************************************
drhand	lda	objst+zstuf+3
		ifeq
			jmp	drtite
		endif
		sta	temp1
		cmp	#01				;At or going to rest point
		ifeq
			lda	daccx
			ifne
				jsr	subx				;Retract to corner
			else
				lda	daccy
				cmp	raccy				;Rest point
				ifne
					ifcc
						jsr	addy
					else
						jsr	suby				;Retract past corner
					endif
				else
					lda	objst+zreactor
					ifmi					;Reactor going to explode
						lda	#02
						sta	objst+zstuf+3		;So turn it back off
					endif
				endif
			endif
		endif
		lda	temp1
		cmp	#02				;Extending to reactor
		ifeq
			lda	daccy
			cmp	maccy
			ifcc
				jsr	addy				;Extend to corner
			else
				lda	daccx
				cmp	maccx
				ifcc
					jsr	addx				;Extend to reactor
				else
					lda	#01				;Return to rest position
					sta	objst+zstuf+3
					lda	retime
					ora	#$80
					sta	reacst				;Set flag so don't award 5000 points again
					lda	#01
					sta	objst+zreactor
					lda	#02
					sta	nxtdisc				;And reset oxygen value
				endif
			endif
		endif
		lda	temp1
		cmp	#03				;Returning to start position
		ifeq
			lda	daccx				;Retract to corner
			ifne
				jsr	subx
			else
				lda	daccy
				ifne
					jsr	suby			;Retract up to box
				endif
			endif
		endif
		ldx	#zstuf+3
		jsr	locate
		tya	
		ifpl
			jmp	drtite
		endif
		lda	vglist
		pha	
		lda	vglist+1
		pha	
		and	#08
		asl	A				;0 if 4000, 40 if 4800
		asl	A	
		asl	A
		asl	A
		sta	perm3				;0 or 80
		sta	vglist
		ora	#$40
		sta	perm3+1			;40 or C0
		lda	#accbuf/$100
		sta	vglist+1			;Accordian buffer
		lda	daccy
		jsr	angcal
		lda	#$F2
		sta	perm4				;One accordian color
		lda	#$E5
		sta	perm4+1			;Other color
		jsr	abuf1
		jsr	swcol
		jsr	abuf1
		jsr	vgrtsl
		lda	perm3+1
		sta	vglist			;Other buffer
		lda	daccx
		jsr	angcal
		jsr	abuf2
		jsr	swcol
		jsr	abuf2
		jsr	vgrtsl
		pla	
		sta	vglist+1
		pla	
		sta	vglist			;Restore vglist
		lda	#$F5
		ldx	#hand7
		jsr	vgadd2			;Purple
		lda	#$30
		ldx	#$71
		jsr	vgadd2			;Scale it
		lda	ghand+2
		ldx	ghand+3
		jsr	vgadd2			;Draw box
		lda	#$F4
		ldx	#hand7
		jsr	vgadd2			;Red
		ldy	#00				;Switch is off
		lda	objst+zstuf+3
		cmp	#03
		ifeq
			ldy	#02				;On
		endif
		lda	ghand+4,Y
		ldx	ghand+5,Y
		jsr	vgadd2			;Draw switch
		lda	#$30
		ldx	#$76
		jsr	vgadd2			;Accordian time!!
		lda	daccy
		ifne
			lda	naccy
			sta	perm1
			begin
				ldx	perm3				;0 or 80
				lda	#accbuf/$100
				jsr	vgjsrl
				dec	perm1
			eqend
		endif
		lda	daccx
		ifne
			lda	naccx
			sta	perm1
			begin
				ldx	perm3+1
				lda	#accbuf/$100
				jsr	vgjsrl
				dec	perm1
			eqend
		endif
		lda	#$30
		ldx	#$71
		jsr	vgadd2			;Now draw hand
		lda	#$F5				;Purple
		ldx	#$63
		jsr	vgadd2
		lda	ghand
		ldx	ghand+1
		jsr	vgadd2
		jmp	drtite
		
angcal	pha					;Get ready to draw accordian pieces in buffer	
		jsr	sin				;Entry value is an angle
		sta	temp1				;Y,+
		jsr	neg
		sta	temp1+1			;Y,-
		pla	
		jsr	cos
		sta	temp2				;X,+
		jsr	neg
		sta	temp2+1			;X,-
		rts	
		
abuf1		lda	perm4				;Draw the vertical accordian buffer
		ldx	#hand7
		jsr	vgadd2			;Set color
		lda	temp2+1
		ldx	temp1+1
		ldy	#$20
		jsr	vgvtr
		lda	temp2
		ldx	temp1+1
		ldy	#$20
		jsr	vgvtr
		lda	perm4+1
		ldx	#hand7
		jsr	vgadd2
		lda	temp2
		ldx	temp1
		ldy	#$20
		jsr	vgvtr
		lda	temp2+1
		ldx	temp1
		ldy	#$20
		jsr	vgvtr
		lda	#00
		ldx	temp1+1
		tay	
		jsr	vgvtr
		lda	#00
		ldx	temp1+1
		tay	
		jmp	vgvtr
		
swcol		lda	perm4				;Switch perm4 and perm4+1
		ldx	perm4+1
		stx	perm4
		sta	perm4+1
		rts	
		
abuf2		lda	perm4				;Assemble horizontal buffer
		ldx	#hand7
		jsr	vgadd2
		lda	temp1
		ldx	temp2+1
		ldy	#$20
		jsr	vgvtr
		lda	temp1
		ldx	temp2
		ldy	#$20
		jsr	vgvtr
		lda	perm4+1
		ldx	#hand7
		jsr	vgadd2
		lda	temp1+1
		ldx	temp2
		ldy	#$20
		jsr	vgvtr
		lda	temp1+1
		ldx	temp2+1
		ldy	#$20
		jsr	vgvtr
		lda	temp1
		ldx	#00
		ldy	#00
		jsr	vgvtr
		lda	temp1
		ldx	#00
		ldy	#00
		jmp	vgvtr
		
subx		ldx	#00				;Subtract angle delta from Y angle
subx2		dec	daccx,X
		ifmi
			inc	daccx,X
		endif
		rts
			
suby		ldx	#01
		jmp	subx2
		
addx		ldx	#00
addx2		lda	daccx,X
		clc	
		adc	#01
		cmp	maccx,X
		ifcs
			lda	maccx,X
		endif
		sta	daccx,X
		rts
			
addy		ldx	#01
		jmp	addx2
		
;********************************************
	.sbttl "Splash for Fireball and Robot"
;********************************************
splash	lda	objst,X
		lsr	A
		lsr	A
		lsr	A
		and	#$1E
		tay	
		lda	shtexp-$10,Y
		ldx	shtexp-$10+1,Y			;Use a shot explosion
		rts	
		
;********************************************
	.sbttl "Stalactites in the Maze"
;********************************************
;* They hang above trip pads and force rex  *
;* to run over the pad. If hit, they vibrate*
;********************************************
drtite	lda	#$C6				;They are all green
		ldx	#tite7
		jsr	vgadd2
		ldx	#ztite+nmtite-1
		stx	temp9
		begin
			lda	objst,X
			ifne
				jsr	locate
				tya
				ifmi
					lda	#00
					ldx	#$71
					jsr	vgadd2			;Scale appropriately
					lda	gtite
					ldx	gtite+1
					jsr	vgadd2
				endif
				ldx	temp9
				lda	objst,X			;Stalactite Status
				cmp	#02
				ifcs
					adc	#00
					cmp	#$40
					ifcs
						lda	#01
					endif
					sta	objst,X			;Repositioning is done in locate
				endif
			endif
			dec	temp9
			ldx	temp9
			cpx	#ztite
		ccend
;********** Fall Through ************************
	.sbttl "Locked Walls in the Maze"
;************************************************
drlock	ldx	#zlock+nmlock-1
		stx	temp9
		begin
			lda	objst,X
			ifne
				jsr	locate
				tya
				ifmi
					jsr	color				;Set color and page
					lda	#00
					ldx	#$71
					jsr	vgadd2			;Set scale
					lda	glock
					ldx	glock+1
					jsr	vgadd2			;Draw it!
				endif
			endif
			dec	temp9
			ldx	temp9
			cpx	#zlock
		ccend
;********** Fall Through ************************
	.sbttl "Keys to fit Those Locks"
;************************************************
drkeys	ldx	#zkeys+nmkeys-1
		stx	temp9
		lda	#$FF
		sta	temp9+1			;Number of keys in Rex's pocket
		begin
			lda	objst,X
			ifne
				cmp	#$10				;In possesion??
				ifcc
					jsr	locate
					tya
					bpl	?key50			;Don't draw it if not on screen
					lda	#00
					ldx	#$71
					jsr	vgadd2			;Set scale
					jsr	color				;Set color and page
					lda	gkey
					ldx	gkey+1
					jsr	vgadd2
				endif
			endif
?key50		dec	temp9
			ldx	temp9
			cpx	#zkeys
		ccend
		jsr	drpod				;Draw the escape pod now
		jmp	onewal			;And the one way walls too!!
		
color		ldx	temp9
		lda	objst,X			;Get color
colr2		and	#$0F				;Entry point for TWMaze key display
		ora	#$E0
		ldx	#lock7
		jmp	vgadd2			;Set color and page
		
;************************************************
	.sbttl "Escape Pod"
;************************************************
;* Entry drpod drawn as static pod.             *
;* Entry drpod2 drawn instead on man picture!   *
;************************************************
drpod		lda	objst+zstuf+2
		ifne
			bmi	?pod10
			cmp	#01
			ifeq
?pod10			ldx	#zstuf+2			;If status is 1 or 80, draw it as static
				jsr	locate
				tya
				ifmi
drpod2				lda	#$68				;Called from Man draw routine in TWMot
					ldx	#$72
					jsr	vgadd2			;Scale down
					lda	objst+zstuf+2		;If in position before blastoff
					cmp	#01
					ifeq
						lda	#$F7
						ldx	#pod7
						jsr	vgadd2			;White
						lda	frame				;Get set for sequencing letters on ship side
						and	#$3C
						cmp	#$14
						ifcs
							lda	#$14
						endif
						lsr	A
						tay
						lda	gpod,Y
						ldx	gpod+1,Y
						jsr	vgadd2
					endif
					lda	#$D9
					ldx	#pod7
					jsr	vgadd2			;Purple Ship
					lda	epodgr
					asl	A
					tay
					lda	gpod+$0c,Y
					ldx	gpod+$0d,Y
					jsr	vgadd2
					lda	objst+zstuf+2		;Flames??
					cmp	#02
					ifcs
						lda	epodgr
						cmp	#04
						ifcc					;Flames!!!
							asl	A
							asl	A
							asl	A
							sta	perm1
							lda	#$F3
							ldx	#pod7
							jsr	vgadd2			;Green Flames
							lda	frame
							and	#06
							clc
							adc	perm1
							tay
							lda	gpod+$1e,Y
							ldx	gpod+$1f,Y
							jsr	vgadd2
						endif
					endif
				endif
			endif
		endif
		rts
		
;****************************************************
	.sbttl "One Way Walls in the Maze"
;****************************************************
;* 	He's no fun - He fell right over!             *
;****************************************************
onewal	ldx	#zonew+nmonew-1
		stx	temp9
		begin
			ldy	#02
			lda	onewst-zonew,X
			ifne
				ifmi
					ldy	#00				;0 points to left, 2 to right
				endif
				sty	temp9+1
				jsr	locate
				tya	
				ifmi
					ldx	#lshot7			;Set sign color (Blue Sign, Yellow Lettering)
					lda	temp9+1
					ifne
						ldx	#lshot7+4
					endif
					lda	#$F1
					jsr	vgadd2			;Also page and xflip
					ldy	temp9+1
					lda	onesign,Y
					ldx	onesign+1,Y
					jsr	vgadd2			;JSRL data in 5000 ROM
					lda	frame				;Now add lettering
					and	#$20
					ifne
						lda	#$C6
					endif
					ldx	#lshot7
					jsr	vgadd2
					lda	onesign+4
					ldx	onesign+5
					jsr	vgadd2
				endif
			endif
			dec	temp9
			ldx	temp9
			cpx	#zonew
		ccend
;********** Fall Through **********************
	.sbttl "Arrows in the Maze"
;**********************************************		
arrow		lda	frame
		and	#$0F
		ifeq					;Next in sequence
			sed					;************* Caution *****************
			lda	thisarw			;One to hilight
			clc	
			adc	#01				;next
			cmp	#$10
			ifcs					;Thats all of them
				lda	#00
			endif
			sta	thisarw
			cld					;***************************************
		endif
		ldx	#zarow+nmarow-1
		stx	temp9
		begin
			lda	objxh,X			;Hit??
			ifne
				ifpl
					jsr	locate
					tya
					ifmi
						ldy	#$AB				;Guess Red
						sec	
						lda	temp9
						sbc	#zarow			;Get which of the arrows
						cmp	thisarw			;One to hilight
						ifeq
							ldy	#$EC				;To flash color
						endif
						ldx	temp9
						lda	ardir-zarow,X
						pha	
						cmp	#09
						ifcc
							tya	
							ldx	#$63
							jsr	vgadd2			;Color the arrow
							lda	#$60
							ldx	#$71
							jsr	vgadd2			;Scale down
							pla	
							asl	A
							tay	
							lda	mazarw,Y
							ldx	mazarw+1,Y
						else
							tya
							ldx	#$61				;Where the out arrows are located
							jsr	vgadd2
							lda	#$60
							ldx	#$72
							jsr	vgadd2
							pla	
							asl	A
							tay	
							lda	mazarw,Y
							ldx	mazarw+1,Y
							jsr	vgadd2
							lda	#$F7
							ldx	#$61
							jsr	vgadd2
							lda	mazarw+$1A
							ldx	mazarw+$1B
						endif
						jsr	vgadd2
					endif
				endif
			endif
			dec	temp9
			ldx	temp9
			cpx	#zarow
		ccend
;********** Fall Through *********************
	.sbttl "Lightning in the Maze"
;*********************************************
ligh		lda	#$EC				;They are flash color
		ldx	#ltg7				;Stat and page select
		jsr	vgadd2
		ldx	#zligh+nmligh+nmfrfl-1
		stx	temp9
		begin
			lda	objxh,X			;Active??
			ifne
				ifpl
					jsr	locate
					tya
					ifmi
						lda	#$38
						ldx	#$72
						jsr	vgadd2			;Scale down
						ldx	temp9
						lda	frame
						lsr	A
						lsr	A
						and	#$0E
						tay	
						cpx	#zfrfl
						ifcc
							tya					;Offset for Horiz
							clc
							adc	#$12				;To H light pics
							tay
						endif
						lda	lightning,Y
						ldx	lightning+1,Y
						jsr	vgadd2
					endif
				endif
			endif
			dec	temp9
			ldx	temp9
			cpx	#zligh
		ccend
;********** Fall Through ***********************
	.sbttl "Trip Plates in the Maze"
;***********************************************
trpplt	lda	frame
		rol	A
		rol	A
		rol	A
		and	#$F0
		ora	#$0B				;Red line
		ldx	#$60
		jsr	vgadd2			;Add color
		ldx	#ztrpp+nmtrpp-1
		stx	temp9
		begin
			lda	objxh,X			;Hit??
			ifne
				ifpl
					jsr	locate
					tya	
					ifmi
						lda	#00
						ldx	#$73
						jsr	vgadd2		;Scale down
						lda	#-$35
						ldx	#00
						jsr	vgvtr5		;Draw blank vector right
						ldy	#$20
						lda	#$66
						ldx	#00
						jsr	vgvtr			;Draw a line back, brightness 20
					endif
				endif
			endif
			dec	temp9
			ldx	temp9
			cpx	#ztrpp
		ccend
;********** Fall Through ************************
	.sbttl "Ship on Maze"
;************************************************
;* Special Routine to draw the ship on the maze *
;* so that the man comes from somewhere.        *
;* It will be assumed that the ship 'always'    *
;* lands at the same place (I zoom it in that   *
;* way!!) So this may be a constant position.   *
;************************************************
shiponm	ldx	#zspecial			;Specials location
		stx	temp3
		lda	#$80				;New position object
		sta	obssxl
		lda	#08
		sta	obssxh
		lda	#00
		sta	obssyl
		lda	#$FC
		sta	obssyh
		lda	#01
		sta	obsst				;Object active??
		jsr	locate			;Place it!!
		tya					;Did it place??
		ifmi
			jsr	shipdis2			;Draw ship out there!!
		endif
;********** Fall Through *************************
	.sbttl "Shots from Robots"
;*************************************************
robshots	ldx	#zshot+nmshot-1		;Number of shots
		stx	temp9
		lda	#shtcol+$F0
		ldx	#mapdt7			;Stat page select
		jsr	vgadd2			;Color shot
		ldx	temp9
		begin
			lda	objst,X			;Active??
			ifne
				ifpl
					inc	objst,X			;Move along!!
					lda	#01
					sta	temp2				;Will move it twice, half distance
					begin					;To ease fast shot collisions
						jsr	movtng			;Moveit!!
						ldx	temp9
						lda	rtcol,ABSX
						ora	ltcol,ABSX			;Hit wall?
						ifmi					;yes
							lda	#$80
							sta	objst,X			;Remove it
						endif
						dec	temp2
					miend
					jsr	locate			;On screen??
					tya
					ifmi					;Yes!
						lda	#$58
						ldx	#$72
						jsr	vgadd2
						lda	vglist+1
						and	#08
						asl	A
						asl	A
						ora	#$80
						ldx	#$A0+(trnbuf&$fff/$200)
						jsr	vgadd2
					endif
				else
					clc
					adc	#08				;Shot explodes
					ifcs
						lda	#00
					endif
					sta	objst,X
					ifne
						jsr	locate
						tya
						ifmi
							lda	#$FB
							ldx	#shtex7
							jsr	vgadd2
							ldx	temp9
							jsr	splash
							jsr	vgadd2
						endif
					endif
				endif
			endif
			dec	temp9
			ldx	temp9
			cpx	#zshot				;Done??
		ccend
;********** Fall Through *********************
	.sbttl "Robots in the Maze"
;*********************************************
robot		ldx	#zrobot+nmrob-1
		stx	temp9
		begin
			lda	objst,X
			bne	?rob5
?rob1			jmp	?rob8				;If 0, skip
?rob5			bmi	?rob1
			lda	objst-zrobot+zshot,X	;Just shoot??
			and	#$3F				;Drop screen bit
			beq	?rob6				;Dead shot, skip it
			cmp	#04
			ifcc
				jsr	colidit			;Just do collisions
			else
?rob6				jsr	movtng			;Move it
			endif
			ldx	temp9				;We will need X again
			lda	velxh,X			;What he did this frame
			sta	temp8+1			;Save for later uses here and in 'drawrob'
			ifmi					;Moving Left
				lda	#00				;If yes, use 'real' vel negated
				sec
				sbc	robvel-zrobot,X
			else
				lda	robvel-zrobot,X		;Real velocity
			endif
			sta	velxh,X			;Guess will move
			lda	ltcol,ABSX
			and	robdir-zrobot,X		;Moving left and hit left wall??
			ifmi					;yes
?rob7				lda	temp8+1			;First time found
				ifne					;yep, we were moving before
					lda	objst+zreactor		;Reactor supercritical??
					ifpl					;no
						lda	objst,X
						and	#$DF				;Set top to close
						sta	objst,X
					endif
				endif
			else
				lda	robdir-zrobot,X		
				eor	#$80
				and	rtcol,ABSX			;Moving right and hit right wall?
				bmi	?rob7
			endif
			jsr	turns				;Check for special change
			lda	frame
			and	#03
			ifeq					;Time to change pic
				lda	objst,X
				and	#$BF				;Drop drawn bit
				bit	mask20
				ifeq
					sec
					sbc	#01				;Close top
					ifeq
						lda	objst+zreactor
						ifmi					;Insert planned random robot turnaround after reactor is set off!
							jsr	getrand
							sta	robdir-zrobot,X		;Creates random direction
						endif
						lda	#$21
					endif
				else					;Counting up
					ldy	objst-zrobot+zshot,X
					ifeq
						ldy	limbo-zrobot+zshot,X
						ifeq
							clc					;Else open top
							adc	#01
							cmp	#$2A				;Check for open all the way?
							ifeq
								jsr	fireshot			;Robot can shoot
								lda	#$0A				;Clear count up flag
							endif
						endif
					endif
				endif
				sta	objst,X			;Restore stat and new pic info
			endif
			lda	rtcol,ABSX
			ifmi
				lda	#00
				sec
				sbc	velxh,X			;Negate
				tay					;Velocity
				ifpl
					ldy	velxh,X			;Use correct signed velocity
				endif
				jsr	obst
			endif
			lda	ltcol,ABSX
			ifmi
				lda	#00
				sec	
				sbc	velxh,X
				tay
				ifmi
					ldy	velxh,X			;Use correct signed velocity
				endif
				jsr	obst
			endif
			jsr	bump					;Bump head
			jsr	locate				;Place it!
			tya						;Did it place?
			ifmi						;yep
				jsr	drawrob
			endif
?rob8			dec	temp9
			ldx	temp9
			cpx	#zrobot
		ccend
?rob10	jmp	drtran

;*************** Special Check for Turns ********************
turns		lda	frame
		and	#01
		ifeq
			lda	objst,X
			and	#$3F
			cmp	#$21				;Opening
			ifeq
				lda	temp8+1			;Standing still??
				ifeq
					lda	objst+zreactor
					ifpl
						lda	robdir-zrobot,X
						eor	#$80
						sta	robdir-zrobot,X
					endif
				endif
			endif
		endif
		rts
			
obst		lda	objst,X
		bit	mask20			;Is it opening??
		ifne					;yes
			and	#$0F				;Check only pic
			cmp	#02
			ifcc					;Hold velocity until turned
				ldy	#00
			endif
		else
			ldy	#00				;Else always hold while closing
		endif
		tya	
		sta	velxh,X
		sta	temp8+1			;Indicate cleared
		ifne					;Velocity change (indicated by tya above)
			bit	objst+zreactor
			ifpl
				sta	robdir-zrobot,X		;Save direction here
			endif
		endif
		rts
			
mask20	.byte $20
mask40      .byte $40

;*******************************************
	.sbttl "Fire a Shot from the Robot"
;*******************************************
fireshot	lda	#01
		sta	objst-zrobot+zshot,X		;Fire a shot
		lda	#$12					;Add some velocity
		ldy	dif4mz
		cpy	#01					;Second maze
		ifeq
			lda	#06					;Yes, slow moving shots
		endif
		ldy	robdir-zrobot,X
		ifmi
			jsr	neg
		endif
		clc	
		bit	objst+zreactor
		ifpl
			adc	velxh,X
		endif
		cmp	#$80
		ror	A					;Moves twice so cut in half
		sta	velxh-zrobot+zshot,X
		lda	#00
		sta	velyh-zrobot+zshot,X
		lda	objxh,X
		sta	objxh-zrobot+zshot,X
		lda	objxl,X
		sta	objxl-zrobot+zshot,X
		clc	
		lda	objyl,X
		adc	#$30					;Offset up to gun
		sta	objyl-zrobot+zshot,X
		lda	objyh,X
		adc	#00
		sta	objyh-zrobot+zshot,X
		lda	objst,X				;Robot on screen??
		asl	A
		ifmi						;yep
			lda	#snd_i7a
			jmp	dosound
		endif
		rts
			
;********************************************
	.sbttl "Transporters in the Maze"
;********************************************
drtran	lda	objst+zreactor
		cmp	#$20
		ifeq
			rts
		endif	
		lda	vglist+1				;Then update the sparkle buffer
		and	#08
		asl	A
		asl	A
		asl	A
		tay						;Position in buffer, 0 or 40
		ldx	#ntrans-1
		stx	temp9					;8 sparkles
		begin
			ldx	temp9
			lda	ttran,X
			clc	
			adc	#01					;Grow and shrink stars first
			cmp	#07
			ifeq						;Need to start a new star
				jsr	getrand
				ora	#$B0					;Get sparkle color
				sta	ctran,X
				jsr	getrand
				begin
					sec	
					sbc	#$19
					cmp	#$19					;Get number 0-18
				ccend
				sbc	#$0B					;Get number -C to +C
				lsr	A
				and	#$1F
				sta	xtran,X				;1A to 6 signed
				jsr	getrand
				begin
					sec	
					sbc	#$2D
					cmp	#$2D					;Get number 0 - 2C
				ccend
				sbc	#$15					;-16 to 16
				lsr	A
				and	#$1F
				ora	#$40
				sta	ytran,X
				lda	#00
			endif
			pha	
			sta	ttran,X
			lda	xtran,X				;Now draw it in buffer
			sta	trnbuf,Y
			lda	ytran,X
			sta	trnbuf+1,Y				;Goto sparkle position
			lda	ctran,X
			sta	trnbuf+2,Y
			lda	#tran7				;Color and page select
			sta	trnbuf+3,Y
			pla	
			tax	
			lda	tranjsrl,X				;Get JSRL location
			tax	
			lda	gtran+6,X
			sta	trnbuf+4,Y
			lda	gtran+7,X
			sta	trnbuf+5,Y				;JSRL to sparkle
			ldx	temp9					;Unless last one, return to center
			ifeq
				sta	trnbuf+6,Y
				lda	#$C0					;RTSL
			else
				lda	#00
				sec	
				sbc	xtran,X				;Return to center
				and	#$1F
				sta	trnbuf+6,Y
				lda	#$A0
				sec	
				sbc	ytran,X				;CS
				and	#$5F
			endif
			sta	trnbuf+7,Y
			tya	
			clc	
			adc	#08
			tay	
			dec	temp9
		miend
		ldx	#ztran+nmtran-1
		stx	temp9
		begin
			lda	objst,X
			ifeq
?trn20				jmp	?trn50
			endif
			and	#$BF
			sta	objst,X
			and	#$20
			ifne
				lda	tspark
				ifne
					jsr	coltran
				endif
			endif
			lda	tspark
			ifne
				lda	tranhi-ztran,X
				bpl	?trn30
			endif
			lda	frame
			and	#03
			ifne
				lda	tranhi-ztran,X
				and	#$7F
				jmp	?trn40
			endif
			lda	tranhi-ztran,X
			ifpl
				ifne
					sec	
					sbc	#01
				endif
				jmp	?trn40
			endif
?trn30		and	#$7F
			clc	
			adc	#02
			cmp	#08
			ifcs
				lda	#08
			endif
?trn40		sta	tranhi-ztran,X
			lda	#00
			ldx	#$61
			jsr	vgadd2
			ldx	temp9
			jsr	locate
			tya	
			bpl	?trn20
			lda	#00
			ldx	#$71
			jsr	vgadd2				;Scale it
			ldy	temp9					;Color like keys and locks
			lda	objst,Y
			and	#$10					;Also get xflip information
			sta	perm1+1
			ifeq
				ldx	#tran7
			else
				ldx	#tran7+4
			endif
			stx	perm1
			lda	objst,Y
			and	#$0F
			ora	#$E0
			jsr	vgadd2
			lda	gtran
			ldx	gtran+1
			jsr	vgadd2
			ldx	temp9
			lda	tranhi-ztran,X
			cmp	#08
			ifeq
				lda	#07					;Pulse transporter base color if activated
			endif
			asl	A
			asl	A
			asl	A
			asl	A
			asl	A
			ora	#$0C
			ldx	perm1
			jsr	vgadd2
			lda	gtran+2
			ldx	gtran+3				;Draw base and top of transporter
			jsr	vgadd2
			lda	#$EC					;Draw bolts flash color
			ldx	perm1
			jsr	vgadd2
			lda	gtran+4
			ldx	gtran+5
			jsr	vgadd2				;Draw bolts
			lda	perm1+1
			ifne
				lda	#00
				ldx	#$60
				jsr	vgadd2
			endif
			ldx	temp9
			lda	tranhi-ztran,X
			beq	?trn50
			asl	A
			asl	A
			sta	temp9+1
			lda	vglist+1				;Choose sparkles from appropriated buffer
			and	#08
			asl	A
			asl	A					;0 or 20
			adc	#$A0					;A0 or C0
			sec	
			sbc	temp9+1
			ldx	#$A0+(trnbuf&$fff/$200)
			jsr	vgadd2				;JSRL to star buffer
?trn50		dec	temp9
			ldx	temp9
			cpx	#ztran
		ccend
		rts	
		
tranjsrl	.byte 0,2,4,6,4,2,0

coltran	cmp	#01
		ifeq
			ldy	#$0F
			lda	#green			;Change sparkle RAM to green if starting to beam
			begin
				sta	colram+$10,Y
				dey
			miend
		endif
		lda	tspark
		sec	
		sbc	#01
		pha	
		and	#$0F
		tay	
		pla	
		and	#$F0
		ifeq
?ctr10		lda	stcolr,Y				;Sparkle in if 00
		else
			cmp	#$20
			beq	?ctr10				;Sparkle in if 20
			ifcs
				lda	#green				;Goto green if 30
			else
				lda	#black				;Goto black if 00
			endif
		endif
		sta	colram+$10,Y
		inc	tspark
		lda	tspark
		cmp	#$41
		ifcs
			lda	objst,X				;No longer being warped to, cause we are there
			and	#$5F
			sta	objst,X
			lda	#00
			sta	tspark
			jmp	initcol				;Restore all colors to normal
		endif
		sta	tspark
		cmp	#$21
		ifeq
			lda	objxh,X
			sta	objxh					;Move man at midpoint
			lda	mazeyh,X
			sta	mazeyh
			lda	#$50					;Need to raise man so he doesn't fall through floor
			sec
			sbc	objyl
			ifcs
				clc
				adc	ymot					;Move man and screen center so no jump
				sta	ymot
				ifcs
					inc	ymot+1
				endif
				lda	#$50
				sta	objyl
			endif
			lda	#$B0
			sec	
			sbc	objyl
			ifcc
				adc	ymot
				sta	ymot
				ifcc
					dec	ymot+1
				endif
				lda	#$B0
				sta	objyl
			endif
			ldy	#00					;Now adjust Y coordinate
			lda	objst,X
			and	#$10
			ifeq
				lda	#$58
			else
				lda	#$A8
			endif
			pha						;Move to edge of transporter on way out
			sec	
			sbc	objxl
			sta	perm1
			ifcc
				dey
			endif
			lda	xmot
			clc	
			adc	perm1
			sta	xmot
			tya	
			adc	xmot+1
			sta	xmot+1				;Adjust screen position of man so transporter stays in the same place visually
			pla	
			sta	objxl					;Move man
		endif
		rts
		
;********************************************
	.sbttl "Bounce Utilities"
;********************************************
;* Bounce off wall... Only changes velocity *
;* if change is needed. else no velocity    *
;* change.                                  *
;*                                          *
;* Inputs: 	velxh,ltcol,rtcol,(x)=obj index *
;*                                          *
;* Output:	update velocity (if needed)     *
;*                                          *
;* Stack:   0                               *
;*                                          *
;* Uses:	A,X(unchanged),temp8	        *
;********************************************
bounce	lda	#00
		sec	
		sbc	velxh,X				;Negated velocity
		sta	temp8
		ifmi						;Negated wants to move left
			lda	rtcol,ABSX				;If -,then want - velocity
			ifmi
?bou10			lda	objst,X
				ifpl						;And not blowing up??
					and	#$10					;temp object?
					ifne
						lda	#$80
						sta	objst,X				;Blow it up!
					endif
				endif
				lda	temp8
				sta	velxh,X
			endif
		else						;Negated wants to go right
			lda	ltcol,ABSX				;Only if left collision
			bmi	?bou10
		endif
		rts	
		
bump		lda	#00
		sec	
		sbc	velyh,X
		tay
		ifmi						;Want to move down??
			lda	headcol,X				;Hit head?
			ifmi
?bmp10			lda	objst,X
				ifpl
					and	#$10				;Temp object??
					ifne					;yes
						lda	#$80
						sta	objst,X			;Blow it up
					endif
				endif
				tya
				sta	velyh,X
			endif
		else						;Want to move up??
			lda	ground,X				;Hit ground??
			bmi	?bmp10
		endif
		rts	
		
;**********************************************
	.sbttl "Robot Routines"
;**********************************************
;* This routine draws the robots in 4 stages. *
;*                                            *
;* 1). The body is always the same.           *
;* 2). The head is next. objst is used to     *
;*     determine which of the 7 head pics is  *
;*     drawn.                                 *
;* 3). The tail is then added. velxh is used  *
;*     to determine which fo 3 tails is added.*
;*     If moving left, tail goes right, etc.  *
;* 4). Then the gun is drawn. this is only    *
;*     drawn if the head is open. Again, objst*
;*     will determine which of 4 gun pics is  *
;*     drawn.                                 *
;* 5). Eyes are last.                         *
;*                                            *
;* Inputs: objst(x),velxh(x), (temp9) = (x)   *
;*         robdir(x-zrobot),(temp8+1)=velxh(x)*
;*                                            *
;* Outputs: vectors to vglist                 *
;*                                            *
;* Uses: A,X,Y,temp7,temp8,temp9              *
;**********************************************
drawrob	lda	#$30
		ldx	#$72				;qqqscalqqq
		jsr	vgadd2
		ldx	temp9				;Object x
		lda	objst-zrobot+zshot,X	;A shot with this robot
		pha					;Save for body decision
		lda	robdir-zrobot,X		;Velocity is flip
		ifpl
			ldx	#body7			;Stat and page select
		else
			ldx	#body7+xflip		;Else flip
		endif
		stx	temp8				;Save this too
		lda	#robcol+$E0			;Add color
		jsr	vgadd2			;Color of body
		lda	temp8+1			;Moving??
		ifeq
			pla					;Recall shot with this robot and toss for now
?dro10		laljsr(body)
			lxhjsr(body)
		else
			pla					;Recall shot with this robot
			and	#$3F				;Drop screen bit
			beq	?dro11			;Skip if not active
			cmp	#$0C				;Just shot??
			bcc	?dro10			;yes, so straighten him up
?dro11		laljsr(bodyt)
			lxhjsr(bodyt)			;Else used tipped body
		endif
		jsr	vgadd2
		jsr	getstat
		cmp	#08				;Top always open
		ifcs
			lda	#08				;Stay open
		endif
		asl	A				;*2 for words
		tay	
		lda	heads-2,Y			;-2 corrects for no 0
		ldx	heads-2+1,Y
		jsr	vgadd2			;Draw head
		lda	temp8+1			;velxh is here
		ifeq
			lda	#00
		else
			lda	#01
		endif
		asl	A				;*2 for words
		tay	
		lda	tails,Y
		ldx	tails+1,Y
		jsr	vgadd2			;Add Tail
		lda	#mancol2+$E0			;Player 2 color for now
		ldx	temp8
		jsr	vgadd2			;Yellow gun
		jsr	getstat
		sec	
		sbc	#04				;Gun out??
		ifpl					;It's out!!
			cmp	#04				;All the way out?
			ifcs
				lda	#03
			endif
			asl	A
			tay	
			lda	guns,Y
			ldx	guns+1,Y
			jsr	vgadd2			;Draw gun
		endif
		jsr	getstat
		cmp	#04
		ifcs
			lda	#02
		endif
		asl	A
		tay
		lda	eyes-2,Y			;-2 as no stat=0
		ldx	eyes-2+1,Y
		jsr	vgadd2
		lda	#blank
		ldx	#$60				;Restore normal stat
		jmp	vgadd2
		
;************* Get object stat and and with 0F ********************
getstat	ldx	temp9				;Recall object index
		lda	objst,X			;Get object status
		and	#$0F
		rts	
		
;********************************************
	.sbttl "Draw and Place Things"
	.sbttl "Locate in the Maze"
;********************************************
;* Ths routine decides if it's time to draw *
;* a 'thing' on the screen or not. This is  *
;* done by checking the 'things' position   *
;* with the current screen location. If     *
;* within certain limits, the object is     *
;* placed on the screen at its location -   *
;* the screen location.                     *
;* Note: The upper left corner is 0,0 and   *
;* the maze is built in the fourth quadrant *
;* so all objects should have X positive    *
;* and Y negative.                          *
;*                                          *
;* Inputs: (x) = object number              *
;*         objxl,objxh,objyl,objyh=location *
;*  If X > ztop then xlsb from table lsb    *
;*  in maze.                                *
;*                                          *
;* Output: (y)= 80 if object placed, else 0 *
;*                                          *
;* Uses:   temp1, xmot                      *
;********************************************
locate	ldy	#00				;Will set it to 80 if placed
		cpx	#ztop				;Special object
		ifcs
			lda	lsbsy-ztop,X		;Get lsb, skip on screen bit
		else
			lda	objst,X
			ifpl					;Skip clear if exploding
				and	#$BF				;Clear displayed bit
				sta	objst,X
			endif
			ifeq
				rts					;Skip if dead
			endif
			lda	objyl,X			;Y low position
		endif
		sec	
		sbc	mazeyl			;Mans Y low position
		sta	temp1				;Difference Y LSB
		lda	mazeyh,X
		sbc	mazeyh			
		sta	temp1+1			;Difference Y MSB
		lda	temp1				;Add in any pan amount
		clc	
		adc	ymot
		sta	xcomp+2			;Store for possible position vector
		sta	temp1
		lda	temp1+1
		adc	ymot+1
		sta	xcomp+3			;Store for possible position vector
		sta	temp1+1
		ifmi
			jsr	dblneg			;Object is below man
			sec
			lda	#$70
			cpx	#zspecial
			ifeq
				lda	#$FF				;Set to max for ship!!
			endif
			cpx	#zcann			;Determine if laser cannon
			ifcs
				cpx	#zcann+nmcann
				ifcc					;Cannon has mount suspended above it
					lda	objyl,X			;We must determine if mount is visible
					eor	#$FF				;So we add in distance to mount to determine the seeable distance
					adc	#$70
					sta	perm2
					lda	#02
					adc	#00
					sta	perm2+1
					lda	perm1				;Starting location Y MSB
					cmp	objyh,X			;If Y MSB is different then it is far away
					ifne
						inc	perm2+1
					endif
					lda	perm2
					sec
					sbc	temp1
					lda	perm2+1
					sbc	temp1+1
					jmp	?loc5
				endif
			endif
			sbc	temp1
			lda	#02
			sbc	temp1+1			;See if less than 320???
		;Exit with sign + if on screen below man
		else					;Check above
			lda	#$30
			sbc	temp1				;See if above 120
			lda	#01
			php	
			cpx	#zstuf+3			;Is it the hand?
			ifeq
				lda	hytend
			endif
			plp	
			sbc	temp1+1
		endif
		
		;One of either of the above sections will return with
		;sign + if the object is on the screen
?loc5		ifmi
			rts
		endif					;On screen Y, now do X
		cpx	#ztop				;Fixed LSB??
		ifcc					;Nope
			lda	objxl,X			;Use given LSB, else...
		else
			lda	lsbsx-ztop,X
			cpx	#ztran			;Check if it is the stalactite
			ifcc					;Yup!
				sta	perm3				;Save the LSB
				lda	objst,X
				cmp	#01
				ifeq
					lda	#00
				endif
				cmp	#$21
				ifcs
					eor	#$3F				;1 to 20, 1E to 1
				endif
				sta	perm3+1
				lda	frame				;Vibrate perm3+1 in an alternating direction
				and	#02
				ifne
					lda	#00
					sec
					sbc	perm3+1
					sta	perm3+1
				endif
				lda	perm3+1
				clc
				adc	perm3				;Yes, thats the LSB
			endif
		endif
		sec	
		sbc	mazexl
		sta	temp1
		ldy	#00				;Reset this in case changed
		lda	objxh,X
		sbc	mazexh
		sta	temp1+1
		clc	
		lda	temp1
		adc	xmot
		sta	xcomp				;Store for possible position vector
		lda	temp1+1
		adc	xmot+1
		sta	xcomp+1			;Store for possible position vector
		ifmi					;Need absolute value
			cpx	#zstuf+3
			ifeq					;hand
				eor	#$FF
				cmp	hxtend
				bcc	?loc15			;Special check
			else
				eor	#$FF				;Check on + side
			endif
		endif
		cpx	#zspecial			;Special object
		ifeq
			cmp	#04
		else
			cmp	#03				;If greater than 3, not on screen
		endif
		ifcc					;On screen, add to list
			cpx	#zfire
			ifcs
				cpx	#zfire+nmfire+nmlsht
				ifcc
					lda	objst,X
					cmp	#$80
					ifeq
						lda	#snd_i7c
						jsr	dosound
					endif
					jmp	?loc14
				endif
			endif
			cpx	#ztop
			bcc	?loc14			;Not special
			cpx	#ztran
			ifcs
				cpx	#ztran+nmtran
				ifcc
?loc14				lda	objst,X			;Add in display bit
					ifpl					;Skip if exploding (is on screen)
						ora	#$40
						sta	objst,X
					endif
				endif
			endif
?loc15		lda	#00
			sta	vgbrit		;Draw it in black
			lda	#$30
			ldx	#$72
			jsr	vgadd2		;Set scale, qqqscalqqq
			jsr	vgcntr
			jsr	vgvtr2		;Place object
			ldy	#$80
		endif
		rts
		
;************************************************
	.sbttl "Move Thing"
;************************************************
;* Will add an objects velocity to its position *
;* and then set collision case flags.           *
;*                                              *
;* Inputs: X = object index                     *
;*         velx(x),vely(x),objx(x),objy(x)      *
;*                                              *
;* Output: Update position, collision flags set *
;*                                              *
;* Uses:   A,X,Y,temp4, 5 bytes stack           *
;************************************************	
movtng	lda	objst+zstuf			;Check clock
		cmp	#02
		ifeq
			lda	frame				;Slow down if set off
			and	#03
			bne	colidit
		endif
movtn2	lda	objst+zstuf+1
		bmi	colidit			;Freeze action for man discovering shoes
		lda	objst+zreactor		;If reactor set off?
		ifmi
			cpx	#zfire
			ifcs
				cpx	#zfire+nmfire
				ifcc					;Yes, it's a fireball
					lda	objst,X
					and	#$10				;Yes it's a permanent one
					beq	colidit			;Don't move it
				else
					cpx	#zrobot
					ifcs
						cpx	#zrobot+nmrob			;Of if is a robot
						bcc	colidit
					endif
				endif
			endif
		endif
		ldy	#00
		lda	velyh,X
		ifmi
			dey
		endif
		clc
		adc	objyl,X
		sta	objyl,X			;Save shot Y LSB
		tya	
		adc	objyh,X
		sta	objyh,X
		lda	objxl,X
		sta	oldxl,X			;Save for collisions
		ldy	#00				;For sign extend
		lda	velxh,X
		ifmi
			dey
		endif
		clc	
		adc	objxl,X
		sta	objxl,X
		tya					;Recall MSB add amount	
		adc	objxh,X
		sta	objxh,X			;Move X position
		
colidit	stx	zindex
		jsr	stloc				;Check for collision
		jsr	docase			;Gets index from 'zindex'
		ldy	zindex
		jsr	topcse			;Check for top line hits
		ldy	zindex
		lda	cktran,Y			;Does it need to be checked against transporters?
		ifmi
			lda	objst+ztran
			ifne					;There are transporters on this maze
				ldx	#nmtran-1
				begin
					lda	objxh+ztran,X
					cmp	objxh,Y
					ifeq
						lda	objyh+ztran,X
						cmp	objyh,Y
						ifeq					;In same square as transporter
							lda	#$80
							sec
							sbc	objxl,Y
							ifmi
								jsr	neg				;Distance from transporter
							endif
							cmp	#$28
							ifcc					;Hit transporter
								lda	objxl,Y
								eor	velxh,Y			;Moving towards transporter
								ifpl
									lda	objst+ztran,X
									asl	A
									asl	A
									asl	A
									eor	velxh,Y
									ifpl					;Warps away
										jsr	tranac
									else
										lda	velxh,Y			;Rebounds off
										ifpl
											lda	#$80
											sta	rtcol,Y
										else
											lda	#$80
											sta	ltcol,Y
										endif
									endif
								endif
							endif
						endif
					endif
					dex
				miend
			endif
		endif
		rts	
		
tranac	lda	objst+ztran,X			;Hit transporter X
		and	#$10
		sta	tempa
		txa	
		eor	#01					;Get other transporter info
		tax	
		lda	objst+ztran,X
		and	#$10
		eor	tempa
		ifeq						;Must reverse object direction
			sec
			sbc	velxh,Y
			sta	velxh,Y
			cpy	#zrobot
			ifcs
				cpy	#zrobot+nmrob
				ifcc
					sta	robdir-zrobot,Y			;Store away direction too if robot
				endif
			endif
		endif
		lda	objst+ztran,X
		pha	
		and	#$40
		ifne
			lda	#snd_i6
			jsr	dosound
		endif
		pla	
		and	#$10
		ifeq
			lda	#$28
		else
			lda	#$D8
		endif
		sta	objxl,Y
		cpy	#zshot
		ifcs
			cpy	#zshot+nmshot
			ifcc
				lda	objst,Y
				clc
				adc	#$10					;Shorter up shot distance if going through transporter
				ifmi
					lda	#00
					sta	objst,Y
					rts
				endif
				sta	objst,Y
			endif
		endif
		txa	
		ora	#08
		sta	limbo,Y
		lda	objst,Y
		sta	stasav,Y
		lda	#00
		sta	objst,Y
		lda	objxh+ztran,X
		sta	objxh,Y
		lda	objyh+ztran,X
		sta	objyh,Y
		txa	
		eor	#01
		tax	
		rts	


	.org $3F80
	
fixit		sta	$900,X
		lda	#01
		sta	rampg
		lda	#00
		sta	$200,X
		sta	$300,X
		sta	$400,X
		sta	$500,X
		sta	$600,X
		sta	$700,X
		sta	rampg
		jmp	patchreturn

	.nocodes		;So we dont have list file buffer overflows
	.fill $4000-*
 	.end
 	
.export init,init2,scbint,gminit,newscore,newbrk,enem0,react,sparks,fire,locate,drpod2,movtn2


