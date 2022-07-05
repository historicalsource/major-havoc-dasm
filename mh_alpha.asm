;***************************************************************
;* Major Havoc MainLine Program (Alpha Processor)              *
;***************************************************************

.locallabelchar "?"

#include "logic.ah"
#include "vector.ah"


#include "mh_vrom.exp"

#include "auxpgm_0.exp"
#include "auxpgm_1.exp"
#include "auxpgm_2.exp"
#include "auxpgm_3.exp"
#include "mh_gamma.exp"

.module alpha
#include "a_ram.ah"

;****************************************************************
;* Defines for Design Time 
;****************************************************************
;Toggle for MAME Emulation, this fixes problems with the Self
;test running during emulation.

MAME = 1

;Sets Beginning Level

STARTLEVEL = $00

;*****************************************************************
;* End Design Settings
;*****************************************************************

	.org $8000
;*****************************************
	.sbttl "TWEB Main Line"
;*****************************************

	.TEXT "COPYRIGHT 1983 ATARI"

pwron   ldx	#$FF
		txs     				;init stack
		sei     	
		lda   #plrslb			;select player 1 for self test
		sta   plysel			;this resets gamma too
		lda   ststsw			;Self Test??
		and   #ststbit			;see if on
		ifeq
			jmp	selftest			;Do self test
		endif
   		lda   #$0D				;Turn on Processors
		sta   out1s				;Set this too
		sta   preset			;Allow Gamma and Beta to Start
;This also sets player Back to 0 (Switch Inputs)
; NOTE:
;    Gamma will reset to send a -1 down the port. No Gamma actions or sounds 
;    will be allowed until that -1 is removed. Also, sends to Gamma will 
;    not cause any effect if this data is not read!

start		lda	#0
		sta	rampg				;Page normal RAM and ROM
		jsr   stpg2
		jsr   init				;Init Memory
		jsr   stpg0	
		lda   #$80
		sta   atflag
		lda   #xmigama			;Wait here
		sta   watchdog
		begin
			bit	ststsw		;See if Gamma sent it's data
		neend
;NOTE: If Gamma in not sending, the doggie will hit us here
		lda	portrd			;Let gamma start
		ifeq
			lda	#xmigama			;wait here
			begin
				sta	watchdog		;okay to wait
				bit   ststsw		;see if gamma sent it's data
			neend				;We can wait
			lda	portstat		
			ora   #$0F				;Drop errors for now
			cmp   #-1			
			ifne
?ml4				bne	?ml4			;Gamma not running correctly stay here!
			endif
		endif
		jsr	hsin				;High Score In
		jsr   inilit
		lda   #snd_j3				;Start Noise
		jsr   dosnd2
		cli  					;Start Progress
		;*************************************
		;this is the start of the main loop
		;*************************************
		begin
			begin   	
				jsr   stpg0
				lda   gamest
				ifmi
?ml39					jmp	playing
				endif
				lda	_crdt			;Credit??
				ifeq
					lda	_cnct
					ifne				;Half Credit??
?ml50						lda	#0
					else
						lda   #1		;Attract, No Credit, Light On
					endif
				else
					lda	_tcmflg		;2 Coin Min??
					bmi   ?ml50			;Hold Light if Waiting
					lda   frame
					rol	A
					rol   A 
					rol   A
					rol   A  
					rol	A			;20 into 1
				endif
				eor	out1s
				and   #1
				eor   out1s
				sta   out1s
				lda   _cmode			;Free Play??
				and   #3				;Look at Coin Bits
				ifeq
					lda	#2
					sta   _crdt		  	;Free Play
					sta   _tcmflg		;Skip This nonsense
				endif
				bit   twocoin			;2 Credit Min??
				ifpl					;yep!
					lda	_crdt				;Got Credit??
					ifeq
						lda	#$80				;Set and wait for 2 credits
						sta   _tcmflg
						jmp   playing			;All for now
					endif
					;Here... We have Credit, A=# of Credits
					cmp	#1				;Just 1, How did we get here?
					bne 	?ml82				;More than 1 we know
					;Here... We have only 1 credit... How did we get one credit
					bit	_tcmflg			;Already Satisfied?
					bmi 	?ml39
				else
?ml82					lda   #0
					sta   _tcmflg
				endif
?ml83			bit	updflg			;Doing Initials??
				bmi   ?ml39				;Skip the Rest Then
				;At this point, okay to do starts
				ldx   _crdt				;Get Credits
				beq	?ml39				;Wait if Credits = 0
				cpx   #2				;At least 2 Credits
				bcc   ?ml1				;1 Player Only
				jsr   getswitch
				asl 	a				;Returned bit into carry
				asl	a				;Returned bit into carry
				rol	swtc2				;Switch 2 Debounce
				lda   swtc2
				and   #07
				ifeq
					jsr   stpg2
					jsr   scbint			;Init Score Buffer
					jsr   stpg0
					lda   #01
					sta   twopl				;Indicate 2 Players
					sta   rampg				;Select High RAM
					sta   player			;Set to Player 1 for Init Stuff
					lda   slives			;Number of Lives per game
					sta   lives+1
					lda   #0
					sta   diedyet			;Clear death flag
					sta   pl2int			;Player 2 has not played
					sta   statst			;Make sure Station is Off
					;(A) Must be zero to call next subroutine
					jsr   zrstuf	
					lda   #$80
					sta   scoflg			;Put up a 0 and lives
					jsr   dscore			;Init Player 2 score
					dec   _crdt				;cost... 1 credit
					jmp	?ml10				;and do Player 1 part too
				endif
?ml1				jsr	getswitch
				asl	a				;Returned bit into Carry
				rol   swtc1				;Switch 1 Debounce
				lda   swtc1
				and   #$07
				ifne
					jmp	playing
				endif
				jsr   stpg2
				jsr   scbint			;Init Score buffer
				jsr	stpg0
				lda	#0
				sta	twopl				;1 Player game
				sta	lives+1			;Make sure 0 lives for Player 2
?ml10				lda	#0
				sta	rampg
				sta	diedyet			;Clear this
				sta	player			;Reset Player Index
				sta	atflag			;Make sure off
				sta	addtim			
				sta	gmov
				sta	gmov+1			;Kill any left over message
				sta   gtime
				sta   gtime+1
				sta   gtime+2			;Clear Game Time
				sta   addmz
				;(A) must be zero when calling next routine
				jsr   zrstuf
				tax     	
				jsr   plrsel			;Set up correctly screen and controls
				lda   slives			;# lives from DIP's
				sta   lives
				jsr   stpg2
				jsr   gminit			;Re-Init Game
				jsr   stpg0
				inc   manstat			;Turn on Man		
;***********************************************
;* Design Time First Level Setting:
;***********************************************
#IF STARTLEVEL != 0
				lda   #STARTLEVEL
				sta	sellvl
#ELSE
				lda   sellvl			;Where to start
#ENDIF
				lsr 	A
				lsr	A
				tax     				;difcty is *4	
				lda   sellvl			
				and   #3
				jsr   thisone
				jsr   gmsti				;Must do this part on startup too!
				jsr   stpg2
				jsr   newscore			
				jsr   incstuf
				jsr   stpg0
				lda   twopl				;One Player Game??
				ifeq
					sta   adddif
					sta   addmn
				endif
				lda   #$80
				sta   gamest			;Start game
				lda   #1
				ora   out1s				;Lamp On
				sta   out1s
				lda   #snd_str			;Start Music
				jsr   dosound
				dec   _crdt				;Cost at least 1 credit
playing			lda	newmzr			;New Maze Request
				ifne
					ldx	player			;Doing Player 2
					ifne					;Yes??
						lda	pl2int			;Has He Played??
						ifeq					;Nope
							lda	chngplr
							bne   ?ml5				;Wait for Select to End
							jsr   stpg2
							jsr   gminit			;Re-Init Game
							jsr   stpg0
							inc   manstat			;Turn on Player
							lda   sellvl			;Where to Start
							lsr   A
							lsr   A
							tax     				;difcty is *4
							lda   sellvl			
							and   #3
							jsr   thisone			;Start him too
							jsr   stpg2
							jsr   newscore
							jsr   incstuf
							jsr   stpg0
							lda   #0
							sta   sellvl
							sta   adddif
							sta   addmn				;Clear for sure now!!
							lda   #$80
							sta   pl2int			;Has he played(or will!!)
							lda   #snd_str			;Start Music
							jsr   dosound
						endif
					endif
					jsr   nextmz
				endif
?ml5			lda   maznum
				cmp   olmznm		;Last Maze Same??
				ifne
					sta   olmznm
				endif
				lda   hcount
				cmp   #20			;Are we waiting this long??
				ifcs
					sta	vgreset		;Stop VG
				endif
				lsr	halt			;Wait for Halt (D0=halt)
			csend
			;************* END INNER LOOP **********************************
			lda	#0
			sta   hcount		;It stopped
			bit   nosync
			ifpl
				begin
					lsr	sync		
				csend			;Wait for IRQ sync
			endif
			sta	watchdog		;Wake up Doggie!!!
			jsr   getswitch		;Get a copy for general frame use
			sta   button
			lda   #0
			sta   nosync		;Clear flag here
			inc   frame			;Another frame
			ifeq
				inc	frame+1
			endif
			lda	demo			;Demo Mode
			cmp	#3			;Demo Mode
			ifeq
				lda	gamest		;Playing??
				ifpl				;Not Playing
					lda	#0
					sta	frame+1		;Hold Here
					sta	addmn			;Allow select of any level
					lda	#3
					sta	addtim
					sta	adddif
				endif
			endif
			lda     	frame
			and     	#0f
			tax     				;Set up Flash color
			lda     	stcolr,X
			sta     	colram+flacol
			lda     	vecram+1
			eor     	#04			;Swap Vector Buffers
			sta     	vecram+1
			sta     	vggo			;And Start
			ldx     	#nmsparks-1		;Fill a few random numbers
			begin
				jsr	getrand
				sta	rands,X
				jsr	dodelay			;Don't clobber NMI's
				dex
			miend
			jsr	seqcon			;Do sequence pic routines
			jsr	stpg2
msk20		jsr	sparks			;Move Fire Ball
			jsr	stpg0
			lda	gamest
			and	#$10				;Clock Hold??
			ifne					;Stop count if sucessful exit
				;Clock hold should never happen in the Maze
				lda	gamest
				bit	msk20			;20 set??
				ifne				;Should not happen
					and	#$af			;Drop Hold bit
					sta	gamest		;Restore
				endif
			else
				bit	mzgame
				ifmi				;Don't count if not Maze
					lda	tottim		;Warning Need for Total Time??
					beq	?ml21			;Not if Empty!!!
					cmp	#$20
					bcs	?ml22			;In case it went back to OK
					lda	frame
					and	#$1F
					ifeq
						lda	#snd_i9		;Low on Oxygen
						jsr	dosound		;Cue up a sound request
					endif
?ml21					lda	frame			;Check for flashing color too!!!
					and	#08
					ifne
						lda	#$F7
					else
?ml22						lda	#timcol+$e0
					endif
					sta	timbuf+02
					lda	incdif
					asl	a
					adc	#$0E
					sta	temp1			;Normal Count Rate
					lda	manstat
					bmi	?ml24			;No Count if Dying
					beq	?ml24			;No Count if Dead
					and	#$20			;Shields On??
					ifne
						asl	temp1			;Cost Oxygen!!
					endif
					lda	tottim+1		;Oxygen LSB
					sec	
					sbc	temp1
					sta	tottim+1
					ifcc
						dec	tottim
						ifeq
							lda	#00
							sta	tottim+1		;Zero LSB as well
							lda	#$80
							sta	manstat		;He will die
						endif
					endif
?ml24					lda	tottim
					jsr	decimal		;Total time in dec to temp7
					lda	#((timbuf+$0A)/$100)&$FF
					sta	vglist+1
					lda	#(timbuf+$0A)&$FF
					sta	vglist
					lda	#temp7
					ldy	#2
					sec	
					jsr	digits		;Display Total Time
				endif
			endif
			sei				;Hold Interrupts Here
			lda	vecram+1
			ldx	#vecram/$100
			and	#04
			ifeq
				ldx	#vecram/$100+8		;Use Upper Buffer
			endif
			lda	#vecram&$ff+2
			sta	vglist
			stx	vglist+1		;Set up next buffer to build in	
			jsr	vgcntr
main2		sei	
			lda	out1s
			ora	#plrslb		;Self Test??
			sta	plysel
			lda	ststsw
			cli	
			and	#ststbit
			ifeq
				jmp	selftest		;Do Self Test
				lda	out1s
				and	#$DF
				sta	plysel
				jmp	loop
			endif
			lda	#00
			sta	xcomp			;Convienient Place to Zero These
			sta	xcomp+1
			sta	xcomp+2
			sta	vgbrit
			ldx	#$71
			jsr	vgadd2		;Set Large Scale
			lda	#01
			sta	xcomp+3		;Set up for Window Vector
			jsr	vgvtr2		;Set Window
			lda	#00
			ldx	#$79			;Set Window, Large Scale
			jsr	vgadd2
			laljsr(scobuf)
			lxhjsr(scobuf)
			jsr	vgadd2
			laljsr(scobuf2)
			lxhjsr(scobuf2)
			jsr	vgadd2		;And 1 to Player 2 Also
			jsr	rdjmp			;Read Jump Button
			jsr	points		;Add in Points
;********************  Game Over and Change Player Messages ************************
			jsr	vgcntr
			ldx	#2
			begin
				stx	temp9			;Check for Game Over
				ldy	gmov-1,X		;May need message
				ifne
					jsr	plrsel		;Make sure pointing correct direction
					dec	gmov-1,X
					ldx	#mgamov		;Game Over
					jsr	mesg
					ldx	#mplayr		;Tell Which Player
					jsr	mesg
					jmp	?ml30
				endif
				ldx	temp9
				dex	
			eqend
			bit	updflg		;Adding to High Score Table??
			ifmi
				jsr	jmp3			;Read Buttons
				jsr	geths			;Get Initials
				lda	frame+1
				ifpl				;Too Late!
					sta	updflg
					sta	updint		;Clear These
					ldx	player		;Set this back if necessary
					jsr	plrsel
					lda	flsflg		;Only write if Top 3
					cmp	#(3*3)
					ifcc
						lda	#-1
						sta	wrhflg		;Write Anyway!!
					endif
				endif
				jmp	loop			;Skip All Else
			else
				ldx	player		;Else Set for Player
				jsr	plrsel
			endif
			lda	chngplr		;Player change message?
			ifne
				lda	adddif
				ora	addmn			;Select Allowed??
				beq	?ml29			;Nope!
				lda	pl2int		;Player 2 not played yet??
				ifeq
					jsr	adddis		;Display Select Message
					jsr	getswitch		;Get Start Switches
					rol	A			;Need CC or +
					bcc	?ml28
					ifpl				;Other??
?ml28						lda	#00
						sta	chngplr
					endif
				else
?ml29					dec	chngplr
				endif			;If Selecting, Dont Count Now
				ldx	#mplayr		;Player Message
				jsr	mesg
				ldx	player
				jsr	plrsel		;Point Correctly
				inx	
				stx	temp9
?ml30				lda	#temp9
				sec				;Display 1 Digit
				ldy	#01
				jsr	digits
				jmp	loop			;And Do nothing else
			endif
;----------------------------------------------------------
		.sbttl "Attract Message Output"
;----------------------------------------------------------
			lda	gamest
			ifmi
				jmp	?ml25
			endif
			jsr	getleta
			pha				;Save Read Data
			ldy	_cnct			;Half Credits??
			cpy	_oldcnt		;Different Than Old??
			bne	?ml32
			ldx	_crdt			;Any Credit
			cpx	_oldc			;Same as last??
			ifne				;No
				stx	_oldc			;Now there are	
?ml32				sty	_oldcnt		;Save old coin count
				lda	#snd_a1		;Klink on coin
				jsr	dosnd2		;Do these even in attract
				ldx	addtim		;In add time??
				beq	?ml31			;Go Back to display if not
				ldx	#09
				stx	addtim		;Else reset timer
				ldx	#00
				stx	frame+1		;And reset frame
			endif
			ldx	addtim		;Doing Add Time??
			ifeq				;If yes, will skip this part
				sec				;Not add a coin, do force stuff
				sbc	atdata		;Turn Right
				ifpl
					cmp	#$10			;Enough to show scores
					ifcs
						ldy	#06			
						sty	frame+1
					endif
				else
					cmp	#-$10
					ifcc
?ml31						lda	frame+1
						cmp	#06			;Already Doing Space??
						ifcs				;Nope
							ldy	#09
							sty	frame+1
							sty	strtst		;Turn Off Story
							sty	jumpst		;Clear any left over jump
						endif
					endif
				endif
			endif
			pla			
			sta	atdata		;In any case, save old read
			lda	frame+1
			cmp	#06
			ifcc
				jmp	?ml35
			endif
			cmp	#08
			ifcc
				jsr	hstbl			;Do High Scores
				jmp	loop			;Skip the rest
			else				;Frame 6,7,8
				ifeq
					lda	frame				
					ifeq					;Start Draw Again??
						sta	seqx					
						sta	seqst				;Start Draw
						sta	logolvl
						sta	strtln			;Will start with line 0
						sta	perm1				;Logo Motion
						jsr	initcol			;Init colors for message
						lda	#$80
						sta	strtst			;Turn on Story
						lda	#-2				
						sta	strtyl
						sta	strtyh			;Start Position
						lda	sndatt			;Attract sounds?
						lsr	A				;Bit 1=yes 0=no
						ifcs
							dec	atsnd				;Next Sound
							ifmi					;Once every 3 times
								lda	rands+1			;Flip a coin
								and	#01
								ifeq
									lda	#02
									sta	atsnd				;1 Out of 3
									lda	rands				;Use a Random Here
									and	#03
									tax
									lda	atmusic,X
									jsr	dosnd2
								endif
							endif
						endif
					endif
				endif
				bit	strtst		;Wait for Story End
				ifpl	
					lda	#00
					sta	manstat		;Be sure dead
					sta	objst+zreactor	;Make sure off
					sta	gamest		;Drop and left over bits
					sta	holmz			;Don't hold any objects
					sta	maznum
					sta	difcty		;Make sure these are 0
					sta	dif4mz		;This must be 0 too!!
					jsr	nextmz		;Set this up!!
					jsr	stpg2
					jsr	init2
					jsr	stpg0
					lda	#03
					sta	spcspd		;Fast stars again
					sta	shipst		;We want the ship on!
					lda	#$80
					sta	atflag
					sta	shldok
					lda	#$40
					sta	lauen			;skip launch
				else
					lda	#00
					ldx	#$71
					jsr	vgadd2		;Scale
					lda	#$AA
					ldx	#$60
					jsr	vgadd2		;Color
					jsr	vgcntr
					ldx	perm1
					lda	#00
					jsr	vgvtr5		;Position
					lda	logolvl		;What level of Name??
					ifeq
						lda	#(havoc1&$ff)
						sta	temp2
						lda	#(havoc1/$100)&$ff
						sta	temp2+1		;Pointer to Name Logo
					else
						cmp	#01
						ifeq
							lda	#$AA
							ldx	#$60			;Set Color
							jsr	vgadd2
							laljsr(havoc1)
							lxhjsr(havoc1)		;Put up First half as jsrl
							jsr	vgadd2
							jsr	vgcntr
							ldx	perm1
							lda	#00
							jsr	vgvtr5		;Position
							lda	#havoc2&$ff		;And scroll in 2nd half
							sta	temp2
							lda	#(havoc2/$100)&$ff	
							sta	temp2+1
						else					;Not Stage 2
							lda	#$AA
							ldx	#$60			;Set Color
							jsr	vgadd2
							laljsr(havoc1)
							lxhjsr(havoc1)		;Put up First Half as jsrl
							jsr	vgadd2
							jsr	vgcntr
							ldx	perm1
							lda	#00
							jsr	vgvtr5		;Position
							laljsr(havoc2)
							lxhjsr(havoc2)		;Put up Second Half as jsrl
							jsr	vgadd2
							jsr	vgcntr
							ldx	perm1
							lda	#00
							jsr	vgvtr5		;Position
							lda	#$CB
							ldx	#$60			;Set Color for Inside Line
							jsr	vgadd2
							laljsr(havoc3)
							lxhjsr(havoc3)		;And put up the last part
							jsr	vgadd2
							lda	perm1
							cmp	#$20			;Mesage all the way up
							ifcs				;Yep!
								ldx	#matar2
								jsr	mesg
							endif
							jmp	?ml33
						endif
					endif
					jsr	copy2			;Do Name
?ml33					lda	seqst			;Did it just finish??
					ifmi				;yep!
						lda	logolvl		;Which Level??
						cmp	#02
						ifcc				;Not Done yet
							inc	logolvl
							lda	#00
							sta	seqx
							sta	seqst
						endif
					endif
					jsr	vgcntr
					lda	logolvl		;In Section 2
					cmp	#02			;In Level 2??
					ifeq
						ldx	perm1
						cpx	#$20
						ifcc
							inc	perm1			;Not There yet
						endif
						lda	#00
						ldx	#$79			;Window!
						jsr	vgadd2
						jsr	stpg3
						bit	strtst		;Which story page??
						ifvc
							jsr	stor3			;Do Scores
						else
							jsr	story
						endif
						jsr	stpg0
					endif
					jmp	loop			;Get Outta here!
				endif
			endif
?ml35			jsr	markjump			;Mec control of attract mode man
			lda	#00
			sta	tactde
			ldx	#mcredi			;Credit Message
			jsr	mesg
			lda	_crdt
			jsr	decimal			;Convert Credits
			lda	#temp7
			sec
			ldy	#02
			jsr	digits			;Display Credits
			lda	_cnct				;Half's??
			ifne
				lda	frame
				and	#$08				;Flash it!
				ifne
					lda	#$FA
					ldx	#$60
					jsr	vgadd2
				endif
				lda	formsg+$1a
				ldx	formsg+$1b			;1/2 Char
				jsr	vgadd2
			endif
		;Display Lives and Bonus
			jsr	doptn
			ldx	#matari
			jsr	mesg				;Copyright Message
			lda	gmov
			ora	gmov+1			;Ending game??
			ifeq
				lda	frame
				and	#$20
				ifeq
					ldx	#mgamov
				else
					ldx	#mpress		;Press Start
					lda	_crdt
					ifeq	
						ldx	#minsert	 	;Insert Coin Message
					endif
					lda	_tcmflg
					ifmi
						ldx	#minsert		;Insert Coin
					endif
				endif
				jsr	mesg			;Game Over
				lda	_cmode
				and	#03
				tay
				ldx	cmodem,Y		;Get Coin Mode Message
				jsr	mesg			;Put it up
			endif
			lda	addtim
			ifne
				lda	adddif
				ora	addmn			;Select to Anywhere??
				ifne
					jsr	adddis		;Do Add display
					jmp	loop			;Just Message
				else
					sta	addtim
				endif
			endif
		;---------------------------------------------
		; End of Attract Stuff 
		;---------------------------------------------
?ml25		jsr	header			;Do Header if Needed
			jsr	vgcntr
			lda	mzgame
			ifmi					;Do Map Now
				ldy	dif4mz			;diff*4+maznum
				cpy	#$0C
				ifcs
					lda	#00
					sta	mtim			;Skip it!
				endif
				lda	mtim			;Maze Hint here
				ifne
					lda	frame
					lsr   A
					ifcc
						dec	mtim			;Time it out!
					endif
					ldx	mazehint,Y		;Get Hint Message
					jsr	mesg
					lda	dif4mz		;diff*4+maznum
					ifeq
						ldx	#mmzh1		;Maze 0, Extra Hint
						jsr	mesg
					endif
				else
					jsr	vgcntr		;Move Up Window
					laljsr(mapbuf)
					lxhjsr(mapbuf)
					jsr	vgadd2		;Draw the map
					jsr	stpg1			;Select Page 1
					jsr	domap			;Should leave us at center
					jsr	stpg0
				endif
				jsr	vgcntr
				lda	#00
				ldx	#$71
				jsr	vgadd2		;Set Scale to 1
				lda	#$B4
				ldx	#$1E			;Normal Window Line
				jsr	vgvtr5
				lda	#whtlin+$f0
				ldx	#$60			;White 
				jsr	vgadd2
				laljsr(longline)
				lxhjsr(longline)
				jsr	vgadd2		;Add White Line
				lda	#00
				ldx	#$79			;Now Set Window
				jsr	vgadd2
				jsr	stpg2
				jsr	react			;Do reactor
				jsr	stpg0
			else
				lda	maznum
				ora	difcty
				ifeq
					lda	mzgame
					cmp	#04			;Landing??
					ifeq
						bit	gamest
						ifvc				;And not leaving
							ldx	#mhint0		;Dock Message
							jsr	mesg
						endif
					endif
				endif
		;-----------------------------------------
		; This should be removable later	
				ldx	#spot7		;Stat ins, page select
				jsr	vgadd2
				laljsr(spot)
				lxhjsr(spot)		;Add spot killer here
				jsr	vgadd2
				jsr	shipout		;Display Ship
				jsr	statot		;Station Control
				lda	tstat
				ifmi
					jsr	strgen		;Do Stars
		;*******************************************************
		;* The Above JSR calls the following Routines:         *
		;*	strgen		generate stars                 *
		;*	strmov		move active stars			 *
		;*	sshots		ship's shots motion            *
		;*	strtshot		start ship's shots             *
		;* 	enemy			Enemy ship's motion call       *
		;*-----------------------------------------------------*
		;* The following subroutines are called by the         *
		;* above list:                                         *
		;*	chkstr		check for star off screen      *
		;*	drawstr		draw an active star            *
		;*	stone			place a shot at ship		 *
		;*	drawshot		draw ship's shots              *
		;*	pastck		enemy ship off bottom?         *
		;*	oneless		remove enemy ship			 *
		;*	drawen		draw and enemy ship		 *
		;* 	shtchk		enemy shot/collision check	 *
		;*	mror	a		utilities for above		 *
		;*******************************************************
				endif
			endif
			jsr	jumpr			;Read Jump Button
			jsr	posit			;Position Object
			jsr	upvel			;Do Velocity
			bit	mzgame		;Maze Game??
			ifmi				;Yes!
				jsr	stpg1
				jsr	trap
				jsr	drawm			;Do Maze
				jsr	stpg2
				jsr	fire			;Position Objects
				jsr	stpg0
		;*******************************************************
		;* The Above JSR calls the following routines:         *
		;*	fire			move fire balls                *
		;*	discs			place discs in maze		 *
		;*	shiponm		place ship 'on' maze           *
		;*	shots			robot's shots motion		 *
		;*	robot			move robots				 *
		;*-----------------------------------------------------*
		;* The Above routines call the following:              *
		;*	turns			turn robots at walls		 *
		;* 	fireshot		fire a shot from robot		 *
		;*	bounce		bounce object off wall         *
		;*	drawrob		draw robot				 *
		;*	locate		draw object in maze		 *
		;*	movthg		move maze object			 *
		;*******************************************************
			else
				jsr	tube			;In Tube?
			endif
loop		jsr	vgcntr
			lda	nogama		;Gamma on Hold??
			ifpl				;Nope
				ldx	#mechs-1		;Any coin counters to send:
				begin
					lda	cntrs,X		;If not 0, send the pulse
					ifne
						lda	whcntr,X		;Which counter to hit
						jsr	xmit			;Hit it
						cli				;Restart Interrupts
						dec	cntrs,X
					endif
					dex
				miend
			endif
			lda	#00
			ldx	#$72
			jsr	vgadd2			;Set Scale
			lda	#$FB
			ldx	#$60
			jsr	vgadd2			;Add Color 
			lda	#$68
			ldx	#-$68			
			jsr	vgvtr5
			lda	wrhflg
			ifmi				;Need to Write High Scores
				jsr	sendhs		;Send them
				lda	#00
				sta	wrhflg		;Request Satisfied
			endif
			jsr	vgcntr
			jsr	vghalt
			lda	#00
			sta	restart		;If here, we can stop restart signal
		neend			;Always
		
;* End of Main Loop                      *
;*****************************************

whcntr	.byte		snd_b2c, g_cn1, g_cn2

zrstuf	sta	tspark
		sta 	objst+zstuf+1
		sta 	objst+zstuf+2
		lda	incdsw
		sta	incdif
		rts	
		
incstuf	ldx	dif4mz
		lda	expdth,X
		sta	incded
		rts	

;*****************************************
	.sbttl "IRQ Rountine"
;*****************************************		
irq		cld	
		pha	
		tya	
		pha	
		txa	
		pha	
		sta	intack
		inc	hcount			;Yet another Interrupt has passed
		lsr	halt
		ifcs					;It stopped!!
			lda	restart			;Continue Flag on??
			ifmi
				sta	vggo			;Start it again
			else				;Not Restart
				lda	vecram+1
				pha
				lda	vecram
				pha			;Save what was there
				laljsr(waste)
				sta	vecram
				lxhjsr(waste)
				stx	vecram+1
				sta	vggo		;A jumpl to waste time
				nop
				nop
				nop
				nop
				nop			;Because Doug says so!!
				pla
				sta	vecram
				pla
				sta	vecram+1
			endif
		endif
		inc	_intct
		lda	_intct
		lsr	A				;Interrupts are every 4ms
		ifcc					;So do coin routine every other time
			lda	out1s
			and	#$DF				;Make sure player 0
			sta	plysel
			jsr	moolah			;Do Coin Routine
			lda	ststsw			;Self Test??
			and	#$10				;See if on
			ifne
				bit	nogama			;On Hold??
				ifpl
					jsr	getleta			;Get LETA data
					jsr	neg
					tay
					sec
					sbc	oldata
					sty	oldata			;Save new and get difference
					cmp	#$80
					ror	A
					clc
					adc	rgdd
					ifvs
						ifmi
							lda	#-$7f
						else
							lda	#$7F
						endif
					endif
					tay
					lda	updflg
					bmi	?irq5
					lda	addtim			;Doing Select??
					bne	?irq5				;Then Keep Running
					lda	gamest			;Game On??
					ifmi					;Yep
						and	#$10
						ifne
							ldy	#00				;Control On Hold
						endif
						lda	mzgame			;No Control in specific states
						and	#$58				;(Like New Jersey)
						ifne
							ldy	#00
						endif
?irq5					sty rgdd
					endif
				endif
			endif
		endif
		lda	_intct			;Count Interrupts
		and	#$07
		ifeq
			inc	sync
			inc	tframe			;True Frame Counter
			ifeq
				sed					;**** Caution Decimal Mode ****
				lda	gtime
				clc
				adc	#01
				sta	gtime
				ifcs					;Only if Carry
					lda	gtime+1
					adc	#00
					sta	gtime+1
					lda	gtime+2
					adc	#00
					sta	gtime+2
				endif
				cld
			endif
		;******* Mark Asked for This!!! *********
		; This is to keep sounds from clobering themselves
			ldx	#02
			begin
				lda	sndcue,X
				ifne
					dec	sndcue,X
				endif
				dex
			miend				;Sound Cue Timers
		endif
?irq10		pla
		tax
		pla
		tay
		pla
		rti	
		

;*****************************************
	.sbttl "Jump Control Status"
;*****************************************
jumpr		bit	gamest		;Leaving Maze??
		ifmi
			ifvc				;Nope, make check
				lda	mazeyh		;Above Maze??
				cmp	#$FC
				bcs	clrjump		;No jumps above maze
			endif
jmpr2			lda	mzgame		;Landing or Just Landed??
			and	#02
			bne	clrjump
			bit	tumble		;Hit Head??
			bmi	clrjump
			lda	objst+zstuf+1	;Are the Magic Boots on?
			cmp	#02
			ifeq				;Yes
				lda	objst			;Man Still Around??
				beq	clrjump		;Don't Jump if he isn't
				bmi	clrjump
				bpl	?jcs10		;Branch
			endif
			bit	mzgrnd		;On Ground??
			ifmi				;Yes
?jcs10			bit	jbstat		;Button Pressed
				ifmi
					lda	#00
					sta	landflg		;Stop Landing
					lda	#$80			;Start Jumping
				else
					lda	#00
				endif
				sta	jumpst			;Clear Jump Bit
			else
				bit	jbstat			;Button Still Pressed??
				ifpl
clrjump				lda	#00
					sta	jumpst			;Clear Jump Flag
				endif
			endif
		else
			bit	markgm		;Find out if control needed??
			ifmi
				lda	markpt		;number of times to do this motion
				ifeq
					lda	markls
					clc
					adc	#02
					sta	markls
					tax
					lda	mazecorr+1,X
					sta	markpt
				endif
				dec	markpt
				ldx	markls
				lda	mazecorr,X
				sta	temp1
				and	#$80
				sta	jbstat		;Jump status in D7
				lda	temp1
				and	#$40
				lsr	A
				sta	temp1+1
				lda	manstat
				and	#$DF
				bit	shldok		;If shields left
				ifmi
					ora	temp1+1		;Shield Status in D6
				endif
				sta	manstat
				lda	temp1
				asl	a
				asl	a
				sta	rgdd			;Rolly-gig data in D0-D5, Signed
				jmp	jmpr2
			endif
		endif
		rts	

mazecorr	.db $00,$03,$07,$02,$08,$13,$81,$18,$88,$04,$0C,$24
		.db $03,$30,$08,$0A,$03,$48,$3C,$20
		.db $00,$40,$B8,$0A,$34,$08,$00,$38
		.db $40,$08,$48,$18,$BF,$40,$B8,$18
		.db $BF,$12,$38,$3C,$B8,$0A,$3C,$40
		.db $08,$0C,$3F,$10,$38,$24,$81,$80
		

;**************************************************
	.sbttl "Markjump - Mec control of spaceman motion"
;**************************************************
markjump	lda	#00
		sta	temp1
		bit	atflag		;In attract mode??
		ifmi
			bit	mzgame		;Inside Maze??
			ifmi
				bit	markgm		;Outside of control last time??
				ifpl
					bit	openflg
					ifpl				;Now inside control??
						lda	#$80
						sta	temp1
						lda	#00
						sta	markpt
						lda	#$FE
						sta	markls
					endif
				else
					lda	mazeyh
					cmp	#$FC
					ifcc				;If inside maze
						lda	#$80
						sta	temp1
					endif
				endif
			endif
		endif
		lda	temp1
		sta	markgm
		rts
		
;*****************************************
	.sbttl "Picture Sequence Control"
;*****************************************
			
seqcon	bit	gamest		;Playing??
		bpl	?psc5			;Skip this Stuff
		lda	manstat		;Check Player Status
		beq	?psc80			;He's Dead Jim!
		bpl	?psc5
		;Dying here
		jsr	clrjump		;Clear Jump Status
		inc	manstat		;Continue to Kill Him
		bne	?psc5			;Just go to scont if not yet dead
?psc80		lda	scrbuf
		ora	scrbuf+1		;Wait till all score is added in
		ifne
?psc5			jmp	scont			;Wait till all score is added in
		endif
		lda	lives
		ora	lives+1		;Anybody Living??
		ifeq
?psc10			lda	#00
			sta	gamest		;Game Over
			beq	?psc5			;Always to scont!!!!!
		endif
		lda	frame
		and	#$3F			;Wait for a frame time period
		bne	?psc5
		;Lost a life if here!!
		lda	#$80
		sta	diedyet		;Indicate he died
		inc	incded
		ldx	player
		lda	lives,X		;Don't go past 0 (Failsafe)
		ifeq
		else
			lda	dif4mz		;(diff*4)+maznum
			asl	a			;*2
			asl	a			;*4
			asl	a			;*8
			ldy	mzgame
			sty	mzgms			;Save what we were doing
			cmp	#$60
			ifcs				;13 and above all store here
				lda	#$60
			endif
			ifmi				;Another maze related death
				clc	
				adc	#04
				tay	
				lda	#$A0
				bit	objst+zreactor		;Reactor set off??
				ifne
					tya
					clc
					adc	#02			;Point to Maze death after reactor
					tay
				endif
				tya				;I told you not to ask
			else
				ifne				;Died Landing??
					clc
					adc	#02
				endif
			endif
			tay
			sed
			lda	spdt,Y
			clc
			adc	#01
			sta	spdt,Y		;Another Maze related death
			lda	spdt+1,Y
			adc	#00
			ifcc
				sta	spdt+1,Y		;Don't let wrap
			endif
			cld
			dec	lives,X		;1 Less Life
			ifeq				;Game Over this player
				lda	twopl			
				ifne				;2 Player Game
					lda	#$7F
					sta	gmov,X		;Put up message 1 sec
				else				;1 Player Game
					ldy	wrplvl		;xfer to Player 2 incase...
					lda	#01			;.. 2 player game is next
					sta	rampg			;RAM Page 1
					sty	wrplvl
					lda	#00
					sta	rampg			;RAM Page 0
				endif
				
				lda	adddif		;See if better
				cmp	difcty
				ifcc				;Yes, Better
?psc90					lda	difcty
					sta	adddif		;This guy was higher.. xfer
					lda	maznum
					sta	addmn
					lda	mzgame
					sta	addmz			;Save this to see where we were
				else
					ifeq					;Same difficulty level
						lda	addmn
						cmp	maznum			;at a different level
						bcc	?psc90			;Yep.... set higher
						ifeq					;If same, set this guy in the maze
							bit	mzgame			;In Maze??
							bmi 	?psc90			;Yep, made it to this level
						endif
					endif
				endif
				lda	demo			;Demo Mode??
				cmp	#03			;Demo Mode
				ifne				;nope
					jsr	stpg0
					jsr	update		;Check for High Scores
				endif
			else
				lda	#$A0
				bit	objst+zreactor	;Reactor set off??
				ifne				;Yep
					lda	#00
					sta	mzgame		;Fake Restart routine
					sta	mzgms			;Store to Mask Too
					jsr	gmsti			;This will make it look like he did good.
				endif
			endif
		endif
		lda	lives
		ora	lives+1		;Recheck for game end
		ifeq				;End of Game
			;Game Just over... check game level
			sta	rampg			;Make sure RAM set
			ldx	wrplvl		;Get Player 1's level
			lda	twopl			;2 Player Game?
			ifne				;Yes
				lda	#01			;Page 1 for Player 2
				sta	rampg
				cpx	wrplvl		;See who greater
				ifcc				;Player 2 was greater
					ldx	wrplvl		;Get Player 2, it was greater
				endif
			endif
		;X = Greater of the 2 players (or Player 1's if only 1 Player Game)
		;We don't know what page where are currently at, so set and store
			lda	#01
			sta	rampg
			stx	wrplvl
			lda	player		;Restore to Correct Page
			sta	rampg
			stx	wrplvl		;Save both
			lda	addmn			;So Back up one level
			sec
			sbc	#01
			and	#03
			sta	addmn
			ifcc				;Dropped a Difficulty Level
				dec	adddif		;Down 1, better not go -
				ifmi				;Oooops!
					lda	#00			;Back to the start sucker!
					sta	adddif		;Stick at 0
					sta	addmn
				endif
			endif
			lda	adddif		;If this is 2 or more, stick at 2,00
			cmp	#03			;Max add-a-coin level is 8(dif=1, maz=3)
			ifcs
				lda	#03			;Above... so stick it back at 2,0
				sta	adddif
				lda	#00
				sta	addmn			;Stick Here (Level 8)
			endif
			lda	addmn
			ora	adddif		;Skip Message??
			beq	?psc11		;Skip if wave 0, level 0
			lda	#$10			;May need this message
			sta	addtim
?psc11			jsr	stats			;Add in stats
			jmp	?psc10			;And get out of here
		endif
		jsr	dostop
		lda	#00
		sta	bonsnd		;Clear Bonus Sound Flag
		lda	twopl			;2 Player Game??
		ifne				;Yes
		;--------------------------------Change Players------------------------------------
?psc1			ldx	#01			;Guess doing player0
			lda	player		;Which Player??
			ifne				;Player 2
				dex				;Set to Player 1
			endif
			stx	player
			stx	rampg			;Select the RAM page
			jsr	plrsel		;Select Player and Controls
			lda	lives,X		;Does he have any lives??
			beq	?psc1			;No, Change Back.
			txa
			ifne				;Yes
				lda	pl2int		;Played Yet??
				ifeq				;No, has not
					sta	sellvl		;Guess he will want level 1
					lda	#$10
					sta	addtim		;Give him some time
					sta	objst			;Turn him on (so this routine skips next time)
				endif
			endif
		endif
		lda	mzgms			;Recall what he was doing
		sta	mzgame		
		lda	#$40
		sta	chngplr		;Put up message
		lda	#00
		sta	toolong		;Clear delay flag
		sta	headcol
		sta	tumble		;Clear man jump status bytes
		lda	#$FF
		sta	newmzr		;Flag for next maze
		sta	frame			;So output happens next
scont		lda	mzgame		
		ifmi				;Do only once!
			lda	gamest		;Sucessful Exit
			and	#$40
			ifne				;Yep
				lda	objxl
				sec
				sbc	#$3D
				lda	objxh
				sbc	#$08			;Ship Enterable from Left or Right
				ifcc
					lda	#00
					sta	direct		;Force Direction
				else
					lda	objxl
					sec
					sbc	#$C3
					lda	objxh
					sbc	#$08
					ifcs
						lda	#$80
						sta	direct
					else
						lda	#00
						sta	velxh			;Stop Running
						lda	xmot
						ora	xmot+1
						and	#$F8			;Done Moving??
						ifeq				;X is done
							lda	ymot
							clc
							adc	#$C0			;See if Y is at 'home'
							lda	ymot+1
							adc	#00
							ifpl				;We are back
								lda	ymot
								sec
								sbc	#$C0
								lda	ymot+1
								sbc	#00
								ifmi
									lda	#02
									sta	mzgame		;Zoom Out
									lda	tottim
									jsr	decimal		;Total Count Into Decimal
									lda	temp7			;lsb
									ldx	temp7+1		;MSB total time count in decimal
									jsr	bpoint		;Add in these too
									lda	#08
									sta	shipyh
									sta	statyh
									lda	#$D0
									sta	shipyl
									sta	statyl
									lda	#center
									sta	shipxh
									lda	#$80
									sta	shipxl		;Ready for takeoff!!
								endif
							endif
						endif
					endif
				endif
			endif
		endif

;****************************************	
	.sbttl "Score & Lives to Buffer"
;****************************************	
dscore	lda	scoflg		;Change??
		ifmi
			lda	#00
			sta	scoflg		;Clear Request
			ldx	player		;Which one
			ifeq
				lda	#((scobuf+$0a)/$100)&$ff
				sta	vglist+1
				lda	#(scobuf+$0a)&$ff
				sta	vglist			;Point to Score Buffer
			else
				lda	#((scobuf2+$0a)/$100)&$ff
				sta	vglist+1
				lda	#(scobuf2+$0a)&$ff
				sta	vglist
			endif
			lda	lives,X
			sec
			sbc	#01		;Count the one we are using
			ifpl				;Skip this in-case minus
				ifne
					cmp	#dislive+1		;Will display max men only
					ifcs
						lda	#dislive
					endif
					sta	temp1			;Count of lives
					begin
						laljsr(lifech)		;Little Man pics
						lxhjsr(lifech)
						jsr	vgadd2		;Add a man
						dec	temp1
					eqend
				endif
			endif
			ldx	player
			lda	#dislive
			sec
			sbc	lives,X		;Leave space for other lives
			ifpl				;Was not 5
				ifne				;In case exactly 5
					sta	temp1
					begin
						laljsr(char_space)
						lxhjsr(char_space)
						jsr	vgadd2		;Spaces
						dec	temp1
					eqend
				endif
			endif
			ldx	player
			ldy	#03
			begin
				txa
				ifeq
					lda	score,Y
				else
					lda	score2,Y
				endif
				sta	temp1,Y
				dey
			miend			;Move score to temp area
			lda	#temp1		;Score moved here
			ldy	#04
			sec
			jsr	digits
			jsr	vgrtsl
		endif
		lda	mazvxl		;Get Velocity
		pha
		eor	direct		;New Direction??
		ifmi
			lda	#00
			sta	scrflg		;Force man to center
			lda	#$80			;Yep, set change request
		else
			lda	#00			;Clear Request
		endif
		sta	dcreq			;Save Request
		pla
		ifmi
			eor	#$FF
			clc
			adc	#01			;Need Absolute Here
		endif
		;***** Assume Normal Conditions, Set Sequence Based on Control *****
		ldy	#runseq		;Guess Running
		cmp	#$18			;At Jog??
		ifcc				;Nope
			dey
			cmp	#$10			;Walk??
			ifcc				;Must be stop
				dey
				cmp	#03			;Stopped??
				ifcc
					dey
				endif
			endif
		endif
		tya
		bit	ground		;On the Ground??
		ifpl				;Nope!
			clc	
			adc	#04			;Guess Jumping.. add 4
			bit	jumpst		;Jumping??
			ifpl				;If not, must be falling
				clc	
				adc	#04
			endif
		endif
		bit	dcreq			;Want to change directions??
		ifmi
			and	#$FC			;Force to Stop
		endif
		sta	picseq		;Next Case
		bit	landflg		;Landing??
		ifmi				;Yes! (Y=primative state)
			cpy	#jogseq		;Skip if Running or Jogging
			ifcc
				lda	#landseq
				sta	picseq
			endif
			lda	piccur		;What picture are we on
			cmp	#12			;Sitting??
			ifeq				;Yes
				lda	sittime		;Help him along
				ifne
					lda	frame
					and	#07
					bit	face			;Want to do it faster??
					ifmi
						and	#03
					endif
					tax				;Set eq flag
					ifeq
						dec	sittime
						ifeq
							lda	#00
							sta	picseq		;Done Sitting, Force to Stop
							sta	tumble		;clear this flag too
							sta	landflg
							sta	face			;Clear your face smash too!!
							lda	#$80
							sta	upflg			;Signal Getting up!
						endif
					endif
				endif
			endif
		endif
		rts
		
		;***********************************************************
		;*  Entry to force start at a particular level             *
		;*  A = mazenum (0,1,2,3)                                  *
		;*  X = difcty  (0,1,2,3,4....)                            *
		;***********************************************************		
		.sbttl "Next Maze Check"
		
thisone	sec	
		sbc	#01			;Set to one before
		ifmi				;Wrapped, set to 3, X minus 1 level
			lda	#03
			dex
		endif
		sta	maznum
		stx	difcty
		lda	#$A0
		sta	objst+zreactor	;We blew the last one!!!!
		lda	#01
		sta	manstat		;Make sure he is on!!
		jsr	stpg2
		jsr	newbrk		;Restore Bricks
		jsr	stpg0
		;Now Fall Through!
		
nextmz	lda	#01			;Worth 100
		sta	nxtdisc		;Restart at 100 points per disc
		jsr	initcol		;Init Colors
		ldy	#$80			;Re-open and outside
		sty	openflg		;A good place to re-signal open
		sty	outflg		;Outside to start always
		sty	shldok		;Shield again
		lda	#$F3			
		sta	retbuf+2		;Turn OK back to Blue
		lda	#03
		sta	shldht		;4 shot hits allowed
		lda	#$A0			;Blownup or set off??
		bit	objst+zreactor	;Did he set off reactor??
		ifne				;yep
			lda	#$80
			sta	tactde		;Then do display this time
			sta	mtim			;Hint message time for maze
			lda	maznum
			clc
			adc	#01			;Next Maze
			and	#03
			sta	maznum
			asl	a
			asl	a
			sta	mazx4			;Save in swap page shadow too
			ifeq				;next diff level
				inc	difcty	
			endif
		;Will do this always in case we may have warped here!!!!
			lda	#00
			sta	trinds		;Save Shadow (for 2 player games)
			sta	jblast		;Debounce Clear (in case left set)
			lda	difcty		;Calculate
			cmp	#maxdif+1		;Don't let go past max
			ifcs
				lda	#maxdif+1
			endif
			asl	a			;-1 as none for maze 0
			tax				;*96 (I hope)
			clc
			lda	#trtbl&$ff
			adc	(x60-2),X			;Add in LSB of table location
			sta	trinds
			lda	#(trtbl&$ff00)/$100	;MSB address
			adc	(x60-1),X
			sta	trinds+1			;Save Table address in shadow
			ldy	#00			;Don't Hold this maze
			sty	holmz			;Will Set up objects
		endif
		lda	trinds		;These may not have changed above
		sta	trind			;Copy over Shadow into 0 page
		lda	trinds+1
		sta	trind+1		;Trip point indirect pointer
		dec	scoflg		;Want to show change
		lda	#03
		sta	objst+zreactor	;Turn on Reactor
		sta	spcspd		;Speed up stars again
		lda	#$20
		sta	olmznm		;Re-Init Objects
		lda	#100d			;Greatest Amount of time
		sta	tottim
		lda	#$FF
		sta	tottim+1		;Reactor Time LSB
		lda	#1
		sta	shipst		;Turn on the ship
		lda	difcty
		cmp	#maxdif
		ifcs
			lda	#03
		endif
		asl	a
		asl	a
		clc	
		adc	maznum		;(difcty*4)+maznum
		sta	dif4mz
		tax	
		cpx	#8
		ifcs
			ldx	#7		;Stick here now
		endif
		lda	landl,X		;Landing width Low
		sta	widthl
		lda	landh,X
		sta	widthh		;Landing width High
		lda	#00
		sta	wrpwh			;Clear this
		sta	wrpdat
		sta	wrpdat+1		;Clear Number
		sta	wrpdat+2
		sta	frame			;Start all new tunnels on even frame
		sta	retime		;Reset reactor time
		sta	sittime		;Stop flipping
		sta	tumble
		sta	mazvxl		;He will stand for a minute
		sta	mazvyl
		sta	nenemy		;None out either
		sta	statst		;Turn off space station
		sta	toolong		;Reset too long timer
		sta	newmzr		;Clear request flag
		sta	perm5			;Reset Breakout Finished this frame flag
		sta	ttime			;Reset Time
		sta	condition		
		sta	lroff			;Reset Offset
		ldx	#colflg-ballx	;Reset Breakout
		begin
			sta	ballx,X
			dex
		miend
		sta	numstr		;No Stars
		ldx	#maxstr-1		;Number of stars
		begin
			sta	strflg,X		;Turn off stars
			dex
		miend
		lda	#01
		sta	stroyh		;Set up initial star origin
		lda	#$78
		sta	stroyl
		lda	gamest
		and	#$8F
		sta	gamest
		bit	mzgame		;In Tact Scan (Could be force wave)
		ifvs				;Yep, jump away now!
			jmp	gmsti
		endif
		ldy	#00
		lda	maznum
		cmp	#spacefort		;This needs it too
		beq	?nm5
		cmp	#fighter
		ifeq				;For fighters
?nm5			lda	#00
			sta	lauen			;Guess don't skip fight
			ldx	#nmspace-1
			begin
				lda	sobjst,X		;See if active
				bmi	?nm10			;If shot, it's dead!
				ifne				;Is it active??
					iny			;yep
				endif	
?nm10				lda	#00			;Now turn it off
				sta	sobjst,X		;Turn off any that might be left
				sta	sobdir,X		;And reset timers
				dex
			miend
			tya
			clc
			adc	nenstr			;In case not all
			sta	nenstr			
			sta	wtcnt				;Also must wait
			ifeq					;There are no more
				bit	gamest
				ifmi				;Only during game play
					lda	#$40
					sta	lauen		;Indicate Skip Fight
				endif
			endif
		endif
		lda	#00
		ldx	#nmspace-1
		begin
			sta	sobjst,X			;Turn all off anyway!!!
			cpx	#nmshot			;Clear old shots too
			ifcc	
				sta	objst+zshot,X
			endif
			dex
		miend
		lda	manstat		;Here from sucessful exit
		ifne				;yep! How about that!
gmsti			lda	#$50			;5000 bonus points available each wave
			sta	bonusa		;Bonus Possible Next time
			ldx	difcty		;Start Number depends on level
			cpx	#4
			ifcs
				ldx	#3			;1 of 3
			endif
			lda	#10d			;Fixed at 10
			ldy	maznum		;Give spacefort only 10
			cpy	#spacefort
			ifne
				lda	howmany,X		;Number of Fighters to start
			endif
			sta	nenstr		;So give him full amount of fighters next time
			sta	wtcnt			;This many waits also
			lda	shotsp,X		;Fighter shot speed
			sta	stsp
			lda	#$80			;If good exit
			sta	init3			;First time in, init this play
			lda	#00		
			sta	lroff			;0 Offset to next entry
			sta	lroff+1
			sta	lauen			;Allow Launch Now
		else				;Sorry Jim... He Died... Beam his body up Scotty
			lda	#01
			sta	manstat		;Turn on Man!
			lda	gamest
			and	#$8F			;Drop Exit and in Maze bits
			sta	gamest
			lda	mzgame
			ifne				;Which Play??
				ifmi				;Was Maze
					lda	#$28			
					sta	shpscl
					lda	#$72
					sta	shpsch		;Set Scale
					lda	#00
					sta	stscll		;Set Base Scale
					sta	shpvel+1		;Stop any motion
					lda	#$73
					sta	stsclh
					lda	#$86
					sta	shipxl		;Postion Ship
					lda	#$04
					sta	shipxh
					sta	mzgame		;Back to quick land
					sta	statxh		;Station's Position
					lda	#$80
					sta	shipyl		;Place Y Position
					lda	#$06
					sta	shipyh		
					lda	#$80
					sta	statxl		;Place Base Ship
					lda	#$08
					sta	statyl
					lda	#$07
					sta	statyh
					lda	#$F9			;Station Status
					sta	statst
				else				;Must have died landing
					lda	#$10
					sta	mzgame
					lda	#$06
					lda	#03
					sta	shipxh
					lda	#$B8
					sta	shipyl
					lda	#$0B
					sta	shipyh		;Place ship
					sta	statyh		;Put station at bottom
					lda	#$80
					sta	statst		;Get it moving
					sta	stbflg
				endif
			else				;Was 0
				lda	#$40			;Back to space (Fake short tube)
				sta	mzgame
				lda	#00
				sta	tcount		;Causes no tube, gives end of tube action however
				jmp	initshp		;Set ship at 'tube postion'
			endif
		endif
		rts
		
;*****************************************	
;* Init Ship to 'Tunnel' Position        *
;*****************************************	
initshp	lda	#00
		sta	shpscl		;Ship's Linear Scale
		lda	#$71
		sta	shpsch		;Ship's Binary Scale
		lda	#$C0
		sta	shipxl		;LSB Positions
		sta	shipyl
		lda	#$05
		sta	shipxh
		lda	#$08
		sta	shipyh		;MSB Positions
		rts	
		
;*****************************************
;* Init Color RAM                        *
;*****************************************
initcol	bit	gamest
		ifmi
			lda	difcty
			cmp	#05
			ifcs
				lda	#00
				sta	colram
				ldx	#$0F
				begin			;Set Sparkle Normally
					lda	stcolr+$10,X
					sta	colram+$10,X
					dex
				miend
				ldx	#$0F
				begin
					jsr	getrand
					and	#$07
					tay			;But Colors are Random
					lda	rncolr,Y
					sta	colram,X
					dex
				eqend
				rts
			endif
		endif
		ldx	#$1F			;Fill Color RAM
		begin
			lda	stcolr,X
			sta	colram,X
			dex
		miend
		rts
		
;****************************************
;* Update Statistics                    *
;****************************************
	.title "A Few Utilities"
	.sbttl "Update Stats of this game"
		
stats		lda	twopl			;0 or 1
		asl	a
		asl	a
		tax	
		sed				;**** Caution ****
		lda	game1,X
		clc	
		adc	#01
		sta	game1,X
		lda	game1+1,X
		adc	#00
		sta	game1+1,X
		lda	game1+2,X
		adc	#00
		sta	game1+2,X
		clc	
		lda	gtime			;Time of this last game
		clc	
		adc	atime1,X
		sta	atime1,X
		lda	gtime+1
		adc	atime1+1,X
		sta	atime1+1,X
		lda	gtime+2
		adc	atime1+2,X
		sta	atime1+2,X
		lda	#00
		adc	atime1+3,X
		ifcc
			sta	atime1+3,X
		endif
		;Histogram Stuff Goes Here!!
		cld	
		rts
		
;*****************************************
	.sbttl "Display Lives and Bonus"
;*****************************************
doptn		ldx	#melife			;Extra Life Message
		jsr	mesg
		lda	#-$40				;Back up and place number
		ldx	#00
		jsr	vgvtr5			;Needs to at this scale
		lda	#-$54
		ldx	#00
		jsr	vgvtr5
		lda	slives			;Number of Lives per game
		sta	temp1
		sec	
		lda	#$11
		ldy	#01
		jsr	digits			;How many lives
		ldx	nxtbonus			;Switch Settings
		lda	bontbl,X			;Get K Settings
		ifne
			pha
			ldx	#mbolif			;Bonus every ...
			jsr	mesg
			pla
			sta	temp1+2			;How Much??
			lda	#00
			sta	temp1
			sta	temp1+1			;0000
			lda	#temp1
			ldy	#03
			sec
			jsr	digits			;Display 100,000
		endif
		lda	twocoin			;Two Coin Minumum
		ifpl
			lda	_tcmflg			;Waiting
			ifmi					;Yep, not satisfied
				lda	_crdt			;Any Credit
				ifne						;1 Credit waiting for second credit here
					lda	frame
					and	#$10					;Flash and Make noise
					ifne
						lda	#snd_b2b				;Make a beep
						jsr	dosnd2
						lda	#$10
					endif
				else
					lda	#$10					;Else always On
				endif
				ifne
					ldx	#mcmod4
					jsr	mesg
				endif
			endif
		endif
		rts
	
;*************************************************
	.sbttl "Read Jump and Shield Buttons"
;*************************************************
rdjmp		lda	gamest
		ifmi
			and	#$10			;Count Hold??
			ifeq				;No
				lda	manstat
				ifne
					ifpl
						and	#$DF				;Guess Cleared
						bit	shldok			;Shields Allowed
						ifmi
							bit	button			;Button pressed
							ifvc
								bit	mzgame
								ifmi
									pha
									lda	#snd_i1a
									jsr	dosound
									pla
								endif
								ora	#$20
							endif
						else
							bit	button			;Pushed this frame?
							ifvc
								bit	jboswt
								ifvc					;Not pushed last frame
									bit	mzgame
									ifmi
										pha
										lda	#snd_i1c
										jsr	dosound
										pla
									endif
								endif
							endif
						endif
						sta	manstat
jmp3						lda	button			;Jump Button
						jsr	neg
						tay
						lda	jboswt			;Old Switch Status
						sty	jboswt			;Save Read Status
						tay
						and	jboswt			;1's where twice 1's
						ora	jbstat
						sta	jbstat			;Save for Main Line
						tya
						ora	jboswt			;0 where twice 0
						and	jbstat
						sta	jbstat
				;swstat will contain last 'Solid-State' case
					endif
				endif
			endif
		endif
		rts
		
;*********************************************
	.sbttl "Math Routines Used All Over"         
;*********************************************
;*                                           *
;* Complement                                *
;*                                           *
;* Entry neg2: Special case for 'targship    *
;*             in module 'twship'	         *
;* Entry neg : Complement                    *
;*                                           *
;* Inputs: (A) = Input                       *
;*         (PS)= Set +/- Correctly for (A)   *
;* Exit:   (A) = Output                      *
;*                                           *
;*********************************************
neg2		ifeq
			iny			;Return FF to 0 for 0
		endif
neg		eor	#$FF
		clc
		adc	#01
		rts
		
;*********************************************
	.sbttl "Double Negate"         
;*********************************************
;*                                           *
;* Will Negate the 2 byte value stored in    *
;* temp1 and temp1+1                         *
;*********************************************	
dblneg	lda	#00
		sec	
		sbc	temp1
		sta	temp1
		lda	#00
		sbc	temp1+1
		sta	temp1+1		;Want a positive integer
		rts	
		
;***********************************************
	.sbttl "Divide by 2*x"         
;***********************************************
;* Does and arithmetic divide by 2 for x times *
;*                                             *
;* CAUTION: Enter with 0 will do 256 times!!   *
;***********************************************	
div2x		begin
			cmp	#$80
			ror	A
			dex
		eqend
		rts

;***********************************************
	.sbttl "Set ROM Page Routines"         
;***********************************************		
stpg0		lda	#00
		beq	stpg
stpg1		lda	#01
		bne	stpg
stpg2		lda	#02
		bne	stpg
stpg3		lda	#03
stpg		sta	rompg
		rts
		
;***********************************************
	.sbttl "Gamma Request Routines"         
;***********************************************	
hsin    sei	
		lda	#$80
		sta	nogama		;Stop Gamma requests
		cli	
		lda	#g_sendh		;Request High Scores First
		jsr	xmit			;Send it away
		ldx	#numhs-1		;Put these away here
		begin
			jsr	rcv			;Get Input
			sta	hscore,X		;Save it
			dex
		miend
		lda	#g_sendi		;Now Get Initials
		jsr	xmit
		ldx	#numit-1		;Number of Initials
		begin
			jsr	rcv
			sta	initl,X
			dex
		miend
		lda	#00
		sta	nogama		;Gamma Stuff allowed again
		rts
		
rcv		lda	#xmigama
		begin
			bit	portstat
		neend
		lda	portrd		;Get Data
		rts
		
dosound	bit	gamest		;Only during game play
		ifmi
dosnd2		jsr	xmit			;Send Data
			cli
		endif
		rts
		
getswitch	sei				;Hold IRQ's please
		lda	#g_swtc		;Request for Switches
		jsr	xmit2			;Do retry xmit and recieve
		cli
		rts
		
getleta	sei				;Hold IRQ's please
		lda	#g_ctrl		;Request for Leta
		jsr	xmit2			;Do retry xmit and recieve
		cli
		rts
			
getrand	sei				;Hold IRQ's
		lda	#g_rand		;Request for Pokey Rand
		jsr	xmit2			;retry xmit and recieve
		cli	
		rts	
		
		
getoptn 	sei				;Hold IRQ's	
		lda	#g_opt1		;Coin Mode Options
		jsr	xmit2
		eor	#$FD
		sta	_cmode		;A Good place to do this
		jsr	dodelay		;Wait for next
		lda	#g_opt0			;Request for Pokey Option Switches
		jsr	xmit2
		cli	
		rts	
		
		
sendhs	sei				;Hold IRQ's	
		lda	#$80
		sta	nogama		;Stop IRQ's from sending
		cli	
		lda	#g_geth		;Send request "gamma get scores"
		jsr	xmit2			;Returns gamma xmit in (A)
		cmp	#01
		bne	sendhs
		;
		;  Ready for xfer to Gamma
		;
		lda	#00
		sta	olddata		;In case something comes back unexpected
		ldx	#00
		begin
			lda	hscore,X
			jsr	xmit			;Send this byte
			sta	watchdog
			jsr	dodelay		;Wait before next send
			lda	olddata		;Not expecting anything back
			ifne				;Got something
				cmp	#02			;Did Gamma Time Out??
				beq	sendhs		;Yep, Try again!
				brk				;Nope, ERROR! Stay here??
			endif
			inx	
			cpx	#numhs
		csend			;All HS sent, should get 'done' back from Gamma
		begin
			lda	#xmigama
			bit	portstat
		neend
		lda	portrd
		cmp	#03			;Okay??
		ifne
			brk				;Not okay
		endif
		sta	watchdog
		jsr	do3del		;Delay * 3
		;
		;All High Scores there okay
		;Now send Initials
		;
sendit	lda	#g_geti		;Now do initials
		jsr	xmit2			;Retry xmit and recieve
		cmp	#01			;Gamma ready??
		bne	sendit		;Well then, try again
		;
		;Ready for Initials
		;
		lda	#00
		sta	olddata		;Nothing here I hope
		ldx	#00
		begin
			lda	initl,X
			jsr	xmit
			sta	watchdog
			jsr	dodelay
			lda	olddata		;Anything?? (I hope not!)
			ifne
				cmp	#02			;Timed out Gamma
				beq	sendit
				brk				;Else, something goofy
			endif
			inx
			cpx	#numit
		csend			;Okay, all sent, now what about status??
		begin
			lda	#xmigama
			bit	portstat
		neend
		lda	portrd		;It sent something
		cmp	#03			;Done??
		ifne
			brk
		endif
		jsr	do3del		;Wait before next
		cli	
		lda	#00
		sta	nogama
		rts	
		
		
xmit2	pha				;Save Request
		jsr	xmit
		begin
			inc	tries
			ifmi
				jsr	resg		;Reset Gamma and Try again
				pla			;Recall what
				jmp	xmit2		;Try again!
			endif
			lda	#xmigama		;Wait for response
			bit	portstat
		neend
		pla				;Toss request away
		lda	portrd
		rts	
		
		;Send Data to Gamma
xmit    sei				;Hold that interrupt please
		pha	
		lda	#00
		sta	tries			;Don't wait too long!
		begin
			inc	tries
			ifmi
				jsr	resg		;Waited too long
				lda	#00
				sta	tries
			endif
			lda	#rcvgama
			bit	portstat		;Buffer full??
		neend			;Wait for Buffer Empty
		pla	
		sta	portwr		;Send Request
		lda	#xmigama
		bit	portstat		;Data Waiting??
		ifne				;Something already came back, old data.
			lda	portrd		;Get the data
			sta	olddata		;Save in old
		endif
		lda	#00
		sta	tries			;0 for return request
		rts
		
			
dostop	lda	#snd_stop
		jsr	dosound		;Stop all sounds
		
		;dodelay - for two sounds in a row
dodelay	sec	
		lda	#$50
		sta	watchdog		;Just in case
		begin
			sbc	#01
		ccend
		rts
			
do3del	ldx	#02
		begin
			jsr	dodelay
			dex
		miend
		rts
			
gdelay	begin
			pha
			pla
			inc	tries
		eqend
		begin
			pha
			pla			;Do Nothing a bit
			inc	tries
		eqend
		rts
		
		;Reset Gamma if No response were expected	
resg		lda	out1s
		and	#$F7			;Reset Gamma Data
		sta	plysel		;Do it
		lda	#$80
		sta	nogama		;Hold Gamma Sends
		lda	out1s
		ora	#08
		sta	plysel		;Hit Gamma in the head
		sta	watchdog
		lda	#00
		sta	tries
		jsr	gdelay
		lda	#rcvgama
		begin
			bit	portstat		;If no response, kill game here!
		neend
		;Gamma will return 0 to indicate talking but not ready!!
		lda	portrd
		bne	resg			;If other than 0, Reset again
?rsg10	lda	#rcvgama
		begin
			sta	watchdog
			bit	portstat		;Gamma is running, we can wait
		neend
		;Got a response.... should be a minus something!!!!
		lda	portrd
		bpl	?rsg10			;No Good, Kill game!
		lda	#00
		sta	nogama		;Ok Gamma, start again.
		rts	
		
;***********************************************
	.sbttl "Add-a-coin(and such) Display"         
;***********************************************
;*  Used to display add-a-coin messages, and   *
;*  also used to set up for select-a-level for *
;*  player 2 start (player 1 at end of game too*
;***********************************************
adddis	ldx	#maddm2
		jsr	mesg
		lda	sellvl
		clc	
		adc	#01			;Display select Level
		jsr	decimal		;Convert
		lda	frame
		lsr	A
		ora	#$A0			;Flash Color
		pha				;Save for below
		ldx	#$60
		jsr	vgadd2
		lda	#temp7
		sec	
		ldy	#01
		jsr	digits		;Which level
		ldx	#maddm1		;Press start within...
		jsr	mesg
		lda	#-$50
		ldx	#00
		jsr	vgvtr5		;Back up to time space
		pla	
		sta	temp3			;Save for select routine
		ldx	#$60			;Get back Flash color
		jsr	vgadd2
		clc	
		lda	addtim
		sta	temp1
		lda	#temp1		;For digit display
		ldy	#01
		jsr	digits		;Display Time
		lda	frame
		and	#$3F			;Seconds (approx)
		ifeq
			sed	
			sec	
			lda	addtim
			sbc	#01
			sta	addtim
			cld
			ifeq
				lda	#00
				bit	gamest			;Game On???
				ifmi					;If yes, this is player 2 select, not restart
					sta	chngplr			;Just do same as pushing start
				else
					sta	adddif
					sta	addmn			;Clear these now
					sta	wrplvl		;Start back at 0
					lda	#01
					sta	rampg
					lda	#00
					sta	wrplvl		;Clear both players
					lda	player		;Restore page to correct one
					sta	rampg
				endif
			endif
		endif
		
;***********************************************
	.sbttl "Select-a-Level Start"         
;***********************************************
;* Used to select a level up to the allowable  *
;* Start level.                                *
;*                                             *
;* Temp3 = Flash color from main line          *
;***********************************************
;* Assumption: This is only called if we can   *
;*             start at more than just level 1 *
;***********************************************
select	jsr	vgcntr
		lda	#$80
		ldx	#-$40			;Position for Display
		jsr	vgvtr5
		lda	#-8
		ldx	#00
		jsr	vgvtr5
		lda	adddif		;Allowable start level
		asl	a
		asl	a
		sec	
		adc	addmn			;Max level to start (+1)
		sta	temp8			;Save this
		tay				;Save for compare
		lda	#01			;Guess start with 1
		ldy	sellvl		;Select level determines this
		cpy	#08
		ifcs
			tya
			sbc	#06			;Start with amount greater than 7
		endif
		sta	temp2			;Number to start with
		lda	#08
		sta	temp2+1		;Do 9 max
		begin			;We are positioned at a place to draw number
			lda	#$FA			;Default Orange
			ldy	temp2
			dey				;It's 1+
			cpy	sellvl		;Number pointed to??
			ifeq
				lda	temp3			;Select Digit.. Flash
			endif
			ldx	#$63			;Flash Digit
			jsr	vgadd2
			lda	temp2
			jsr	decimal		;Convert
			lda	#temp7
			sec
			ldy	#01			;Display This
			jsr	digits
			lda	#$11
			ldx	#00
			jsr	vgvtr5		;Position for next digit
			dec	temp2+1		;Do all Max??
			bmi	?sal10
			inc	temp2
			lda	temp8
			cmp	temp2			;Reached max??
		ccend
?sal10		lda	temp3
		ldx	#$63			;Always flash arrow
		jsr	vgadd2
		dec	temp8
		jsr	vgcntr
		lda	sellvl		;Which level selected
		cmp	#08
		ifcs
			lda	#07			;Decrease for Offset
		endif
		asl	a
		asl	a
		asl	a
		asl	a
		pha				;Save for double position
		clc	
		adc	#-$7a			;Offset to char at left
		ldx	#-$58			;Position for arrow
		jsr	vgvtr5
		pla	
		sec	
		sbc	#01			;In case it is 80!
		ldx	#00			;No Y motion
		jsr	vgvtr5		;Complete Postion
		lda	#00
		ldx	#$71
		jsr	vgadd2		;Size of ship
		lda	mazarw+4
		ldx	mazarw+5
		jsr	vgadd2
		;Now Read in the Roller
		ldy	#00
		jsr	rgdr			;Get and move data
		asl	a
		asl	a
		ifmi
			dey				;Prop sign
		endif
		clc	
		adc	wrpdl			;Can use these here
		sta	wrpdl
		tya	
		adc	sellvl		;Move Level
		ifmi
			lda	#00
		endif
		cmp	temp8			;Max side
		ifcs
			lda	temp8
		endif
		sta	sellvl
		rts
		
;***********************************************
	.sbttl "Initialize Option Switches"         
;***********************************************	
inilit	sei	
		lda	out1s
		ora	#plrslb		;Add in Player Select
		sta	plysel		;Get 2 coin min
		ldx	twocoinmin		;In cabinet bit
		lda	out1s
		ora	#$FD
		sta	plysel		;Reset
		cli	
		txa	
		asl	a			;Put 2coin in D7
		sta	twocoin		;Read Option
		jsr	getoptn		;Get Gamma Options
		;Above will set _cmode according to a bank of switches
		;and return second set of option switches in (A)
		eor	#$23			;So all off is default
		tay	
		and	#01
		sta	addap			;Addaptive Difficulty
		tya	
		lsr	A
		and	#01
		sta	sndatt		;Sounds in Attract
		tya	
		lsr	A
		lsr	A
		and	#03			;Bonus Life bottom 3 bits
		eor	#02			;00=100K standard
		sta	nxtbonus		;This is the place
		tya	
		lsr	A
		lsr	A
		lsr	A
		lsr	A			;Next two bits = Difficulty
		and	#03
		eor	#03
		sta	demo
		cmp	#03
		ifeq
			lda	#00
		endif
		sta	incdsw		;Save incremental Difficulty
		tya				;Get options again
		rol	A
		rol	A
		rol	A			;Top two bits
		and	#03
		sta	temp1			;Save this a sec
		lda	_cmode
		and	#03
		asl	a
		asl	a			;*4 for table entry, 4 each level
		clc	
		adc	temp1			;Get Lives
		tax	
		lda	gamlvs,X
		sta	slives
inilt2	lda	#(engmsg&$ff00)/$100		;MSB
		sta	litral+1
		lda	#(engmsg&$ff)			;LSB
		sta	litral
		jsr	stpg3					;These are in Page 3
		lda	lngtb2+1
		sta	litra2+1				
		lda	lngtb2
		sta	litra2				;Story Pointer
		lda	lngtb3+1
		sta	litra3+1
		lda	lngtb3
		sta	litra3
		jmp	stpg0
		
;***********************************************
	.sbttl "Misc Tables"         
;***********************************************
		
atmusic	.byte snd_mys, snd_brk, snd_str, snd_hsc

bontbl	.byte	$00,$05,$10,$20

gamlvs	.byte $02,$05,$04,$03,$03,$06,$05,$04,$03,$06,$05,$04,$05,$07,$06,$04

cmodem	.byte mcmode, mcmod1, mcmod2, mcmod3

landl		.byte $10,$00,$C0,$B0,$A8,$A0,$98,$90

landh		.byte $01,$01,$00,$00,$00,$00,$00,$00

mazehint	.byte mmzh0, mmzh2, mmzh3, mmzh4
		.byte mmzh5, mmzh6, mmzh7, mmzh8
		.byte mmzh9, mmzha, mmzhb, mmzhc 

howmany	.byte 16, 25, 30, 30

shotsp	.byte $14,$1C,$24,$28

stcolr	.byte black
		.byte blue
		.byte green
		.byte cyan
		.byte red2
		.byte purple
		.byte yellow
		.byte white
		.byte whiter
		.byte pink
		.byte orange
		.byte redr
		.byte red
		.byte cyanr
		.byte bluer
		.byte greenr
		
	
		;Sparkle RAM Color Inits
		.byte	$02,$0F,$0F,$08,$0F,$0F,$07,$0F
		.byte $0F,$0A,$0F,$0F,$06,$0F,$0F,$00
		
rncolr	.byte blue
		.byte green
		.byte cyan
		.byte yellow
		.byte white
		.byte orange
		.byte redr

x60		.word $00,$60,$c0,$120,$180

cksum5	.byte $81

;***********************************************
.title "TWUTIL"         
;***********************************************
	.sbttl "Cosine Routine"
;* cos(a)=sin(A+PI/2)                    *
;*****************************************
cos		clc	
		adc	#$40		;Fall through to Sin now
		
;******************************************
	.sbttl "Sine Routine"
;******************************************
;* (A)=Angle (0 to FF represents 0 to 360 *
;* (CC+=Minus Plus Flag Set Correctly     *
;*                                        *
;* Exit: (A)=sine (-127 to +127)          *
;*                                        *
;* Uses: (A),(X)                          *
;******************************************
sin		ifmi				;if pi>(A)>0
			and	#$7F
			jsr	sin1			;sin(a) when pi>a>0
			jmp	neg
		endif
		
sin1		cmp	#$41
		ifcs				;pi/2>a>0
			eor	#$7F			;sin(pi/2+a)=sin(pi/2-a)
			adc	#00
		endif
		tax	
		lda	sincos,X
		rts
			
sincos	.byte 000,003,006,009,012,016,019,022
		.byte 025,028,031,034,037,040,043,046
		.byte 049,051,054,057,060,063,065,068
		.byte 071,073,076,078,081,083,085,088
		.byte 090,092,094,096,098,100,102,104
		.byte 106,107,109,111,112,113,115,116
		.byte 117,118,120,121,122,122,123,124
		.byte 125,125,126,126,126,127,127,127
		.byte 127
		
;**********************************************
	.sbttl "Multiply"
;**********************************************
;* Multiply - Unsigned. Need to conditionally *
;* fix the result for the signed operations.  *
;* Uses Nibble method from 4 sums.            *
;* (A4*B4*100)+((AH*BL+BL*AH)*10)+(AL*BL)     *
;*                                            *
;* Input	(A)=signed                        *
;*          (temp1)=unsigned (preserved) (=B) *
;*                                            *
;* Output	(temp2,temp2+1), High byte in A   *
;*                                            *
;* Uses	X,Y,A,temp4,temp2                 *
;**********************************************
multiply	tax	
		php			;Reset Status
		;
		;Form AH*BH*100
		;
		sta	temp4
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		eor	temp1
		and	#$0F
		eor	temp1
		tax	
		lda	multbl,X
		sta	temp2+1
		;
		;Form AL*BL
		;
		lda	temp4
		asl	a
		asl	a
		asl	a
		asl	a
		eor	temp1
		and	#$F0
		eor	temp1
		tax	
		lda	multbl,X
		sta	temp2
		;
		;Form AL*B4 Index and save in Y
		;
		lda	temp4
		eor	temp1
		and	#$0F
		eor	temp1
		tay	
		;
		;Form AH*BL, Save in X
		;
		lda	temp4
		eor	temp1
		and	#$F0
		eor	temp1
		tax	
		lda	multbl,X
		clc	
		adc	multbl,Y
		tax	
		ror	a
		lsr	a
		lsr	a
		lsr	a
		sta	temp4
		txa	
		asl	a
		asl	a
		asl	a
		asl	a
		clc	
		adc	temp2
		sta	temp2
		lda	temp2+1
		adc	temp4
		plp		;If +, ok as it is.	
		ifmi		;If -, subtract the unsigned value from the high byte of results
			sec
			sbc	temp1
		endif
		sta	temp2+1
		rts
		
;*********************************************
	.sbttl "Full Log Table to Convert Ship Sizes"	
;*********************************************
fullog	.byte $7F,$7F,$7E,$7D,$7C,$7C,$7B,$7A,$7A,$79,$78,$77,$77,$76,$75,$74
		.byte $74,$73,$72,$71,$71,$70,$6F,$6E,$6D,$6D,$6C,$6B,$6A,$69,$69,$68
		.byte $67,$66,$65,$64,$64,$63,$62,$61,$60,$5F,$5E,$5E,$5D,$5C,$5B,$5A
		.byte $59,$58,$57,$56,$55,$55,$54,$53,$52,$51,$50,$4F,$4E,$4D,$4C,$4B
fullog2	.byte $4A,$49,$48,$47,$46,$45,$44,$43,$42,$41,$40,$3F,$3E,$3D,$3C,$3B
		.byte $3A,$38,$37,$36,$35,$34,$33,$32,$31,$30,$2E,$2D,$2C,$2B,$2A,$29
		.byte $28,$26,$25,$24,$23,$22,$20,$1F,$1E,$1D,$1C,$1A,$19,$18,$17,$15
		.byte $14,$13,$11,$10,$0F,$0D,$0C,$0B,$0A,$08,$07,$05,$04,$03,$01,$00
		
;*****************************************************
	.sbttl "Quarter Log Table to Draw Unfinish Web Lines"	
;*****************************************************
qrtlog	.byte $FD,$F9,$F6,$F2,$EF,$EC,$E8,$E5,$E1,$DE,$DA,$D7,$D3,$CF,$CC,$C8
		.byte $C4,$C1,$BD,$B9,$B6,$B2,$AE,$AA,$A6,$A3,$9F,$9B,$97,$93,$8F,$8B
		.byte $87,$83,$7F,$7B,$77,$73,$6F,$6B,$66,$62,$5E,$5A,$55,$51,$4D,$49
		.byte $44,$40,$3B,$37,$33,$2E,$2A,$25,$21,$1C,$17,$13,$0E,$09,$05,$00
		
;*****************************************************
	.sbttl "Multiply Table"	
;*****************************************************		
multbl	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F
		.byte $00,$02,$04,$06,$08,$0A,$0C,$0E,$10,$12,$14,$16,$18,$1A,$1C,$1E
		.byte $00,$03,$06,$09,$0C,$0F,$12,$15,$18,$1B,$1E,$21,$24,$27,$2A,$2D
		.byte $00,$04,$08,$0C,$10,$14,$18,$1C,$20,$24,$28,$2C,$30,$34,$38,$3C
		.byte $00,$05,$0A,$0F,$14,$19,$1E,$23,$28,$2D,$32,$37,$3C,$41,$46,$4B
		.byte $00,$06,$0C,$12,$18,$1E,$24,$2A,$30,$36,$3C,$42,$48,$4E,$54,$5A
		.byte $00,$07,$0E,$15,$1C,$23,$2A,$31,$38,$3F,$46,$4D,$54,$5B,$62,$69
		.byte $00,$08,$10,$18,$20,$28,$30,$38,$40,$48,$50,$58,$60,$68,$70,$78
		.byte $00,$09,$12,$1B,$24,$2D,$36,$3F,$48,$51,$5A,$63,$6C,$75,$7E,$87
		.byte $00,$0A,$14,$1E,$28,$32,$3C,$46,$50,$5A,$64,$6E,$78,$82,$8C,$96
		.byte $00,$0B,$16,$21,$2C,$37,$42,$4D,$58,$63,$6E,$79,$84,$8F,$9A,$A5
		.byte $00,$0C,$18,$24,$30,$3C,$48,$54,$60,$6C,$78,$84,$90,$9C,$A8,$B4
		.byte $00,$0D,$1A,$27,$34,$41,$4E,$5B,$68,$75,$82,$8F,$9C,$A9,$B6,$C3
		.byte $00,$0E,$1C,$2A,$38,$46,$54,$62,$70,$7E,$8C,$9A,$A8,$B6,$C4,$D2
		.byte $00,$0F,$1E,$2D,$3C,$4B,$5A,$69,$78,$87,$96,$A5,$B4,$C3,$D2,$E1

;*****************************************************
	.sbttl "Score Routines"	
;*****************************************************
;* Add Points Passed in to a buffer that will be     *
;* 'scrolled' into the total score                   *
;*                                                   *
;* Entry: (A)= LSB Amount, (X)=MSB Amount            *
;*****************************************************
bpont2	ldx	#00			;For places where X=0
bpoint	sta	scbdis+1
		stx	scbdis+2		;Will Display this amount
bpont3	bit	gamest		;Enter here to skip display of this amount
		ifmi
			sed
			clc
			adc	scrbuf
			sta	scrbuf
			txa			;Add MSB
			adc	scrbuf+1
			sta	scrbuf+1
			cld
		endif
		lda	#$0F			;3 1/2 Seconds Approx
		sta	scbdis+3		;Display Time
		rts

;*****************************************************
	.sbttl "Add Points to Score"	
;*****************************************************
;* Add Points to players score                       *
;*                                                   *
;* Entry: (scrbuf(2))= hundreds to add               *
;*****************************************************		
points	txa	
		pha	
		tya	
		pha				;Save X
		ldx	#00			;Guess Player 0
		stx	temp5			;0 Amount
		lda	player		
		ifne
			ldx	#score2-score	;Point to Player 2 Score
		endif
		lda	scrbuf+1		;Alot of Points
		ifne
			inc	temp5
		endif
		lda	scrbuf
		ifne
			inc	temp5
		endif
		lda	temp5			;Any Score??
		ifne
			dec	scoflg		;X, score change
			sed				;**** decimal mode ****
			clc
			adc	score+1,X
			sta	score+1,X		;Add in Thousands
			ifcs				;A Carry!
				lda	#00
				adc	score+2,X
				sta	score+2,X
				php				;Save any Carry
				ldy	nxtbonus,abs	;0 is no free lives
				ifne
					cpy	#01
					ifeq
						and	#$0F		;1 is one every 50K
						beq	?aps10
						cmp	#05
						beq	?aps10
					endif
					cpy	#02
					ifeq
						and	#$0F
						beq	?aps10
					endif
					cpy	#03
					ifeq
						and	#$1F
						ifeq
?aps10						cld				;Skip Decimal for a bit
							txa
							pha				;Save X
							ldx	player
							inc	lives,X		;Another Life
							lda	#snd_c4
							jsr	dosound		;Extra Life sound
							sed				;Reset Decimal
							lda	extlie
							clc
							adc	#01
							sta	extlie
							lda	extlie+1
							adc	#00
							ifcc
								sta	extlie+1
							endif
							pla
							tax				;Restore Index
						endif
					endif
				endif
				plp				;Restore any carry
				lda	#00
				adc	score+3,X
				sta	score+3,X
			endif
			lda	scrbuf
			sec
			sbc	temp5			;How Much we added
			sta	scrbuf
			lda	scrbuf+1
			sbc	#00
			sta	scrbuf+1
		endif
		cld				;Clear Decimal
		ldx	scbdis+3		;Any Display time??
		ifne
			lda	frame
			and	#03
			ifeq
				dec	scbdis+3
			endif
			txa	
			asl	a
			asl	a
			asl	a
			asl	a
			ora	#05		;Cyan to dim out
			ldx	#$60
			jsr	vgadd2	;Add Stat
			lda	#00
			ldx	#$72
			jsr	vgadd2	;Scale
			jsr	vgcntr
			lda	#-$58		;Guess Player 1
			ldx	player
			ifne
				lda	#$4C		;Nope, on other side (player 2)
			endif
			ldx	#$60
			jsr	vgvtr5	;Position
			lda	#00
			sta	scbdis	;Fake the 2 extra 0's
			lda	#scbdis
			sec
			ldy	#03
			jsr	digits	;Display this
		endif
		pla
		tay
		pla	
		tax	
		rts
;*****************************************************
	.sbttl "Digits - Display 2 Digits"	
;*****************************************************
;* Display 2Y digit numbers                          *
;* Input	(C) = carry set for zero supression      *
;*          (A) = address of (Y) zero page           *
;*          (Y) = number of zero page locs to use    *
;*                                                   *
;* Uses	A,X,Y,temp5                              *
;*****************************************************	
digits	php			;Save input parameters
		dey	
		sty	temp5+1	;Y may be a constant
		clc	
		adc	temp5+1
		sta	temp5		;MSB of Digits
		plp	
		tax
		begin
			php	
			lda	vgbrit,X
			lsr	A
			lsr	A
			lsr	A
			lsr	A
			plp	
			jsr	vghexz	;First Digit
			lda	temp5+1
			ifeq
				clc			;Display Last Digit Even if 0
			endif
			ldx	temp5
			lda	vgbrit,X
			jsr	vghexz	;Second Digit
			dec	temp5
			ldx	temp5
			dec	temp5+1
		miend
		rts
		
;*****************************************************
	.sbttl "Decimal Conversion"	
;*****************************************************
;* Hex to BCD decimal conversion                     *
;* Input:	A                                        *
;* Output:	temp7    = MSB                           *
;*          temp7+1  = LSB                           *
;* Uses: 	A,Y,temp5                                *
;*****************************************************			
decimal	sta	tempa		;Save this
		ldy	#07		;Bit Count
		lda	#00
		sta	temp7		;Clear Results
		sta	temp7+1
		sed			;***** Warning *****
		begin
			asl	tempa
			lda	temp7
			adc	temp7
			sta	temp7
			lda	temp7+1
			adc	temp7+1
			sta	temp7+1		;An 'inc' to Decimal
			dey
		miend
		cld			;***** Okay Now *****
		rts
		
;*****************************************************
;*****************************************************
	.title "TWCoin"
	.sbttl "Coin Routine"	
;*****************************************************
;* Coin65 - 650X Universal Coin Routine              *
;*                                                   *
;* Programmers: Downend & Albaugh                    *
;* Revised for Havoc 10-27-83                        *
;* Removed all macro references 6-13-00              *
;*****************************************************
	
include 	= 1
bonadd	= 1
coin67	= 1	;High 3 bits
slam		= 0	;Assembled Low
cntint	= 0	;ill count interrupts myself
gama		= 1	;Tell my coin routine gamma is here
	

moolah	ldx	#02		;X is used to index from right to left coin mech
_detct	lda	ststsw	;Get Coin Switches
		cpx	#01		;Which mech are we doing
		beq	?c11		;Middle Shift Twice
		bcs	?c12		;Right Shift Once
		asl	a		;Else left, shift Thrice
?c11		asl	a
?c12		asl	a
		lda	_cnstt,x
		and	#$1F		;Shared Inst
		bcs	?c5		;Branch if input high (Coin Absent)
		beq	?c1		;Stick at 0(Terminal Count)
		cmp	#$1B		;In First 5 Samples
		bcs	?c10		;Yes, run fast
		tay			;Else, save status
		lda	_intct	;Check Interrupt Counter
		and	#07		;Are D0-D2 all 1's??
		cmp	#07		;Set Carry if so
		tya			;Status back into A	
		bcc	?c1		;Skip if not all 1's
?c10		sbc	#01		;Carry Set
?c1		sta	_cnstt,x	;Save updated Status
		lda	ststsw	;Check Slam Switch
		and	#$10		
		bne	?c2		;Branch if bit hi (switch off)
		lda	#$F0		;Else set pre-coin slam timer
		sta	_lmtim	;Dec 8 times/frame = prst frames
?c2		lda	_lmtim	;Check pre-coin slam timer	
		beq	?c3		;O.K.
		dec	_lmtim	;else run timer
		lda	#00		
		sta	_cnstt,x	;Clear Coin Status
		sta	_pstsl,x	;Clear Post-Coin Slam Timer
?c3		clc			;Default 'no coin detected'		
		lda	_pstsl,x	;check post-coin slam timer
		beq	?c8		;empty, proceed
		dec	_pstsl,x	;run timer
		bne	?c8		;not done, prceed
		sec			;when it becomes zero, indicate a coin
		bcs	?c8		;(always)
?c5		cmp	#$1B		;Is coin valid yet?? (On for >4 samples)
		bcs	?c6		;no,reset it
		lda	_cnstt,x	;Get status again
		adc	#$20		;Bump coin-off up counter
		bcc	?c1		;If it didn't wrap, just store status
		beq	?c6		;It wrapped but coin was on too long, just reset
		clc			;Set validity again
?c6		lda	#$1F		;Reset Down Counter
		bcs	?c1		;Branch if coin too long or too short
		sta	_cnstt,x	;Save reset status
		lda	_pstsl,x	;Check Howie's assumption
		beq	?c7		;Branch if $pstsl vacant
		sec			;Else give credit a little early	
?c7		lda	#$78		;Post Frames
		sta	_pstsl,x	;Delay acceptance for post/60 sec.
?c8		bcc	?c9

	.sbttl "Mech-Multipliers"
		lda	#00		;Start with 0
		cpx	#01		;Check which mech
		bcc	?c85		;If left, always add 1
		beq	?c83		;If center, check half mult
		lda	_cmode	
		and	#$0C
		lsr	A
		lsr	A
		beq	?c85		;00- Add 1
		adc	#02		;Else map 1,2,3 to 3,4,5
		bne	?c85		;(always)
?c83		lda	_cmode	;Get Coin Mode from Zero Page
		and	#$10		;Isolate half multiplier
		beq	?c85
		lda	#01
?c85		sec	
		pha	
		adc	_bccnt	;Update Bonus Adder Counter
		sta	_bccnt
		pla	
		sec	
		adc	_cnct
		sta	_cnct
		inc	cntrs,X	;Shadow Counter
?c9		dex	
		bmi	_bonus
		jmp	_detct
		
	.sbttl "Bonus Adder"
_bonus	lda	_cmode
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		lsr	A		;Isolate Bonus-Adder mode in bits 0-2
		tay	
		lda	_bccnt
		sec	
		sbc	_modlo,Y	;See if enough unit-coins have accumulated
		bmi	_cnvrt	;Branch if not
		sta	_bccnt	;Else update Bonus Adder and...
		inc	_bc		;Give 1 or 2 Bonus Coins
		cpy	#03		
		bne	_cnvrt
		inc	_bc		;Mode 3 yields 2 bonus coins for 4 inserted
		bne	_cnvrt	;branch
		
_modlo	.byte $7f,$02,$04,$04,$05,$03,$7f,$7f	;7f generates 0 bonus coins

	.sbttl "Convert Coins to Credits"
_cnvrt	lda	_cmode	;Get Coin Mode
		and	#03		;Isolate Coins-Credits Option
		tay	
		beq	?cc2		;If free play, cmode=0, do nothing
		lsr	A		;Else for price (0,1,1,2)
		adc	#00
		eor	#$FF		
		sec	
		adc	_cnct		;Coinct-Price -> A
		bcs	?cc3		;Branch if no borrow
		adc	_bc		;Add in bonus coins, see if they help
		bmi	ext		;Branch if coinct+bonus coins < price
		sta	_bc		;Else A = Unused bonus coins
		lda	#00
?cc3		cpy	#02		;Y=coin mode - Coin mode 2 or 3??
		bcs	?cc1		;Branch if mode 2 or 2, give 1 credit
		inc	_crdt		;Else give 2 for mode 1
?cc1		inc	_crdt	
?cc2		sta	_cnct		;Update coinct
;Fall Through to handle EM counters
ext		lda	_intct	
		lsr	A		;Use LSB for pulse
		bcs	?cc9	
		ldy	#00		;Start with flag of 0
		ldx	#02		
?cc4		lda	_cctim,X	;Check Timer(X)
		beq	?cc6		;Neither running nor pending
		cmp	#$10		;Is it running
		bcc	?cc6		;No, skip
		adc	#$EF		;else dec 4 MSB
		iny			;Set on Flag
		sta	_cctim,X	
?cc6		dex			
		bpl	?cc4
		tya			;Check "on" Flag
		bne	?cc9		;Skip if any on
;If any of the counters are currently on, we check to see if any can be started
		ldx	#02
?cc7		lda	_cctim,X	;We need to start this one
		beq	?cc8		;No, no counts pending
		clc	
		adc	#$EF		;Set 4 MSB, Dec 4 LSB
		sta	_cctim,X	;Start Timer
		bmi	?cc9		;Exit, so we don't start more
?cc8		dex	
		bpl	?cc7
?cc9		rts	

;*********************
; End of Coin Routine
;*****************************************************

;*****************************************************
	.title "Vector Routines"
;*****************************************************


	
vgrtsl	lda	#$C0
		ifeq
vghalt		lda	#$20
		endif
		ldy	#00
		sta	(vglist,Y)
		jmp	vgvctr2
vghexz	ifcs
			and	#$0F
			beq	?vgj10
		endif
vghex		and	#$0F
		clc	
		adc	#01
?vgj10	php	
		asl	a
		ldy	#00
		tax	
		lda	vgmsga,X
		sta	(vglist,Y)
		lda	vgmsga+1,X
		iny	
		sta	(vglist,Y)
		jsr	vgadd
		plp	
		rts
			
vgjmpl	lsr 	A
		and 	#$1F
		ora	#$E0
vgjsr1	ldy	#01
		sta	(vglist,Y)
		dey
		txa
		ror	A
		sta 	(vglist,Y)
		iny
		bne	vgadd
vgjsrl	lsr	A
		and 	#$1F
		ora	#$A0
		bne	vgjsr1
		ldy	vgbrit
vgstat	ldx 	#$60
		begin
			tya
			jmp	vgadd2
			ldx	#$60
		eqend	
vgcntr	lda	#$40
		ldx	#$80
vgadd2	ldy	#00
		sta	(vglist,Y)
		iny	
		txa	
		sta	(vglist,Y)
vgadd		tya	
		sec	
		adc	vglist
		sta	vglist
		ifcs
			inc	vglist+1
		endif
		rts	

		
vgsca1	ldy   #$00
vgscal	ora	#$70
		tax	
		tya	
		jmp	vgadd2
vgvtr5	ldy	#00
vgvtr		sty	vgbrit
vgvtr1	ldy	#00
		asl	a
		ifcs
			dey
		endif
		sty	xcomp+1
		asl	a
		rol	xcomp+1
		sta	xcomp
		txa	
		asl	a
		ldy	#00
		ifcs
			dey
		endif
		sty	xcomp+3
		asl	a
		rol	xcomp+3
		sta	xcomp+2
vgvtr2	ldx	#$27
vgvctr	ldy	#00
		lda	vglist+1,X
		sta	(vglist,Y)
		lda	litral,X
		and	#$1F
		iny	
		sta	(vglist,Y)
		lda	vgbrit,X
		iny	
		sta	(vglist,Y)
		lda	vglist,X
		eor	vgbrit
		and	#$1F
		eor	vgbrit
vgvctr2	iny	
		sta	(vglist,Y)
		bne	vgadd
		
;*****************************************************
	.title "TWMot - TWEB Motion"
;*****************************************************

offpos	= -6		;Man's off screen position


chkpic	.byte 29d,29d,29d,31d,31d,31d,33d,33d		;Choking Pictures

;***********************************************************
	.sbttl "Place and Output Picture"
;***********************************************************
;* This routine sets up and adds the man picture to the    *
;* display list. It also times when the next picture       *
;* change should take place based upon the current         *
;* picture sequence in progress, and calls nextpic         *
;*                                                         *
;* Inputs:	scal,frame,piccur,sittime,direct,picseq,picdely*
;*                                                         *
;* On Exit: Adds JSRL to VGLIST, updates picdely,sittime   *
;*                                                         *
;* Stack: 	2 bytes (JSRL call)                            *
;***********************************************************
newpic	lda	objst
		ifeq
			rts
		endif
		lda	#02			;Scale 1 for position
		ldy	#$30			;Linear Part
		bit	mzgame,abs	
		ifpl				;Not in mazegame if +
			ifvs				;In Tube??
				ldy	#$94
			else
				rts
			endif
		endif
		jsr	vgscal		;Set Scale
		jsr	vgcntr		;Center
		ldy	#00
		sty	vgbrit		;Blank Vector
		ldx	#xmot			;Vector from xmot
		jsr	vgvctr		;Place this object
		lda	piccur
		cmp	#stoppic		;Stopped??
		ifeq
			lda	picseq		;Stopped??
			ifeq				;Yes
				sta	upflg			;Standing, clear flag
				lda	frame
				and	#01
				ifeq
					inc	waitim		;Still waiting??
					ifeq
						dec	waitim		;Don't allow to pass back to 0
					endif
				endif
				lda	waitim
				ifmi				;Long Enough??
					lda	rtcol,abs
					ora	ltcol,abs		;Near Wall??
					ifmi
						lda	#wtpic2
					else
						lda	#wtpic1		;We shall wait
					endif
					sta	piccur		;Use this picture now
				endif
			else
				lda	#$40
				sta	waitim		;Reset Wait Time
			endif
		endif
		bit	mzgame,abs		;No Shield in Space!!
		ifmi
			lda	manstat
			ifpl				;No Shield if dying
				bit	msk20_2		;Shields On??
				ifne				;Yes
					lda	#snd_i1a		
					jsr	dosound		;Shield Sound
					lda	#$FF			;Brightness
					ldx	#shld7+sparkle	;Sparkle too
					jsr	vgadd2
					lda	frame
					ifmi
						eor	#$FF
						and	#$7F
					endif
					lsr	A
					lsr	A
					clc
					adc	#$50			;Min Size
					tay
					lda	#02
					jsr	vgscal
					laljsr(shield)
					lxhjsr(shield)
					jsr	vgadd2
				endif
			endif
		endif
		lda	mestim
		ifne
			jsr	idiotd		;Display Idiot Message If needed
		endif
		ldy	#$68			;Set Linear Size
		lda	mzgame,abs		;In Tube??
		ifpl				;If +, Not in Maze
			ldy	#$98			;Scale
		endif
		lda	#02			;At Scale 2
		jsr	vgscal		;Add Scale
		lda	piccur
		cmp	#wtpic1		;Waiting??
		ifeq				;Yep
			lda	teeter		;In a teeter position?
			ifne				;Could be
				ora	#$80			;Make it look that way
				sta	teeter
			endif
		endif
		lda	objst+zstuf+1
		ifmi
			lda	#$FB			;Show Boots on Man
			ldx	#boot7
			jsr	vgadd2
			lda	gboot+$0a
			ldx	gboot+$0b
			jsr	vgadd2
			jmp	boots2
		endif
		lda	objst+zstuf+1		;Maybe also show man levitating on force field
		cmp	#02
		ifeq
			lda	jumpst			;Check button pressed unless in demo mode
			ifmi
boots2			lda	#$AB
				ldx	#boot7
				jsr	vgadd2
				lda	frame
				and	#03
				asl	a
				tay
				lda	gboot+2,Y		;Four Frame Sequence
				ldx	gboot+3,Y
				jsr	vgadd2
			endif
		endif
		bit	face				;Slamming Face into Wall??
		bmi	bad4				;Yes, so skip below
		lda	frame
		ldy	jumpst			;Jumping??
		ifmi					;Yep
			ldy	#00
			sty	upflg				;Obviously not getting up'
			ldy	piccur
			cpy	#jumppic			;Jumping??
			ifeq					;yep
				and	#$10				;Flip Bit select
				lsr	A
				lsr	A
				ora	#mpic7			;Page Select
			else
				jmp	bad4				;(Bad Habit) Do Normal Pic
			endif
		else					;No Jumping, Sitting?
			bit	teeter			;Or maybe Teetering
			ifpl					;nope, not teetering
				ldy	sittime
				ifne					;Sitting on Floor??
					and	#xflip
					ora	#mpic7
				else
bad4					lda	#mpic7		;Stat base and page select
					ldy	direct
					ifmi
						ora	#xflip		;Add X Flip
					endif
				endif
			else
				and	#$3C			;Mask to alt teeter and not teeter
				lsr	A
				lsr	A
				tay
				lda	teetpc,Y		;Change direction every 4 frames
			endif
		endif
		ldx	piccur
		cpx	#wtpic2			;Waiting??
		ifeq					;yep
			ldx	rtcol,abs			;Right Collision??
			ifpl					;No, lean other way
				ora	#04
			endif
		endif
		bit	manstat			;Is he dying??
msk10_2	ifmi	
			ldy	tottim			;Death due to lack of air??
			beq	?npi15
			ora	#sparkle			;Add Sparkle to old stat
			tax
			lda	manstat
			and	#$F0
			eor	#$F0
		else
?npi15		tax					;Just save A as is
			lda	#mancol+$e0			;Add color and intensity
			ldy	player			;Player 2??
			ifne					;yes change color
				lda	#mancol2+$a0		
			endif
		endif
		sta	temp5				;Save for Later (Maybe)
msk20_2	jsr	vgadd2			;Add Page Select
		ldx	picseq			;Get Sequence
		dec	picdely			;Picture Delay
		ifmi					;Ready for next pic??
			lda	deltbl,X			;New Delay Amount
			sta	picdely
			jsr	nextpic
		endif
		lda	manstat			;Dying??
		ifmi
			ldy	tottim			;Due to Lack of Oxygen??
			ifeq					;Yep!
				pha					;Save this
				sec
				sbc	#$80
				lsr	A
				lsr	A
				lsr	A
				lsr	A
				tax
				lda	chkpic,X		;Get Choking Pics
				tax				;Save pic
				pla				;Recall manstat
				cpx	#33d
				ifne				;Last one is fixed
					and	#08			;Flip Code
					ifne
						inx
					endif
				endif
				stx	piccur
			else
				ldy	#stoppic
				sty	piccur
				and	#$F0
				eor	#$F0			;Want him to fade out
				eor	frame
				and	#$F0
				eor	frame			;Flash Color
				ldx	#mpic7
				jsr	vgadd2		;Add Color and Stat
				laljsr(pic34)
				lxhjsr(pic34)
				jmp	vgadd2		;Add Skeleton Pic
			endif
		endif
		;*************** Now output picture to list *************************
		lda	piccur			;Current Pic
picout	ldy	objst+zstuf+2		;Escape pod out there?
		ifpl
			cpy	#02				;Climbing??
			ifcs
				lda	#02
				sta	rompg
				jmp	drpod2			;Draw Escape Pod instead of man
			endif
		endif
		ldy	objst+zstuf+1		;Magic Boots
		ifmi					;Freeze is on, Man w boots on is displayed
			iny
			cpy	#$B0
			ifcs
				ldy	#02			;Leave this mode after 20 frames
				lda	#$A0
				sta	gamest		;Return to normal play (non-freeze)
			endif
			sty	objst+zstuf+1
			lda	#$15			;Show a specific man picture
		endif
		bit	tumble		;Clear tumble bit in strange situations
		ifmi
			cmp	#wtpic1	;Is he tapping his foot??
			beq	?op10
			cmp	#24d		;Just standing there
			ifeq
?op10				inc	unstik			;Another Frame Stuck
				ldx	unstik
				cpx	#07				;After a certain number of frames, clear tumble
				ifcs
					ldx	#00
					stx	tumble
				endif
				jmp	?op20			;Don't Clear Unstick
			endif
		endif
		ldx	#00			;No lockup condition this frame, clear unstik
		stx	unstik
?op20		cmp	#wtpic1			;Prevent man on side of screen on tube
		ifne
			bit	mzgame,abs
			ifvs					;Make sure a man in attract
				tay
				lda	ymot
				sec
				sbc	#$55
				lda	ymot+1
				sbc	#$FE
				bpl	?op30			;Make sure a man above a certain height
				tya	
			endif
		endif
		pha	
		asl	a			;Times 2 for index use
		tay	
		bit	teeter
		ifmi
			ldy	#38*2			;Teeter picture if teetering
		endif
		lda	mansrc,Y		;Get Source
		ldx	mansrc+1,Y
		jsr	vgadd2		;Add man's pic
		pla
		cmp	#wtpic1		;Tapping Foot??
		ifeq				;Yes
			bit	teeter		
			ifpl				;not teetering
				lda	frame
				and	#$0F
				ifeq
					bit	mzgame,abs		;In Tube??
					ifvc				;Skip if in tube
						lda	#snd_i8		;'tap' foot
						jsr	dosound
					endif
				endif
				lda	frame
				and	#08
				ifeq
					laljsr(leg1)
					lxhjsr(leg1)
				else
					laljsr(leg2)
					lxhjsr(leg2)
				endif
				jmp	vgadd2
			endif
		endif
?op30		rts

;***********************************************************
	.sbttl "Next Picture"
;***********************************************************
;* This routine gets the next picture in the sequence      *
;* and handles and special case hold requests.             *
;*                                                         *
;* Entry:	Current pic number (piccur)                    *
;*                                                         *
;* Exit:	New pic code updated                           *
;***********************************************************	
nextpic	bit	landflg
		ifmi
			lda	piccur
			cmp	#stoppic		;Stop pic??
			ifeq				;yes
				bit	stopflg		;Force to stop seq (from Land)
				ifmi
					lda	#00
					sta	picseq
					sta	stopflg		;Clear flag
					sta	landflg		;Not landing either
				endif
			else
				cmp	#00			;Stride
				ifeq
bad8					sta	landflg
					lda	#runseq
					sta	picseq		;Force to run
				else
					cmp	#stripic		;Other stride
					beq	bad8
				endif
			endif
		endif
		lda	picseq		;Current Sequence
		asl	a			;*2 for words
		tax				;into X
		lda	pictbl,X		;Get Low byte of Table Address
		clc	
		adc	piccur		;Add Current Pic
		sta	picind		
		lda	pictbl+1,X
		adc	#00			;Prop carry if one (High Byte)
		sta	picind+1		;Create Indirect Pointer for this seqs table
		ldy	#00
		lda	(picind,Y)		;Get Next Pic
		ifmi				;Possible Foot Sound
			and	#$7F			;Drop foot bit
			cmp	piccur		;Already at this pic?
			ifne				;Not the same
				pha				;Save pic code
				lda	#snd_i8		
				jsr	dosound
				pla
			endif
		endif
		sta	piccur	
		bit	gamest		;Skip this if in tube
		ifvc
			cmp	#stoppic		;Stopped??
			ifeq
bad6				lda	#00
				sta	dcreq
				lda	rgdd			;Get new direction
				and	#$80			;Only direction please
				sta	direct		;Update direction
			else
				cmp	#wtpic1		;Waiting??
				beq	bad6
				cmp	#wtpic2
				beq	bad6
			endif
		endif			;Fall through....
;***********************************************************
	.sbttl "Update Velocity"
;***********************************************************
;* Updates X and Y velocities using Rolly-Gig data         *
;* and jump button flags                                   *
;***********************************************************		
	
upvel		lda	mzgame,abs		;Doing Maze??
		and	#$4D
		ifne				;Doing Tube, skip this
?uv1			rts
		endif	
		lda	gamest		;Skip this on completed maze
		and	#$48			;Succesful exit or just entered??
		bne	?uv15			;Will skip X velocity if yes
		lda	tumble		;Did he hit his head??
		ifne				;yep
?uv10			lda	#00			
			sta	velxh			;Stop X Motion
?uv15			jmp	noact			;And do nothing else
		endif
		bit	upflg			;Getting up??
		bmi	?uv10			;Yep, don't allow to move
		lda	manstat		;Only when active
		bmi	?uv10
		beq	?uv10			;2 cases of no action
		;X Update
		lda	mazvxl		;Are we moving??
		ifmi
			clc
			adc	#01			;-1 check
		endif
		ifne
			lda	#$60			;Reset Wait Timer
			sta	waitim		;He moved
			lda	piccur
			cmp	#wtpic1		;Waiting??
			ifcs				;yes
				lda	#stoppic
				sta	piccur
			endif
		endif
		lda	rgdd
		bit	mzgrnd		;On ground??
		ifpl				;nope
			lda	mazvxl
			ldx	#04
			jsr	div2x			;1/16 of velocity
			sta	temp1			;Save
			lda	mazvxl
			sec
			sbc	temp1			;A = 7/16 of old velocity
			sta	temp1			;Hold this
			lda	rgdd			;Control Velocity
			ldx	#03			;Use 1/16 Control
			jsr	div2x
			clc	
			adc	temp1			;mazvxl(new)=7/8(mazvxl(old)+1/8(Control_velocity)
			ifvc				;No Overflow??
				sta	mazvxl		;Store new
			endif
		else
			clc
			adc	mazvxl
			bvs	?uv20			;Already wrapped, max out
			clc
			adc	rgdd			;else, add data in again
			ifvc				;No overflow in speed
				cmp	#$80
				ror	A
			else				;Stick at max
?uv20				ifmi				;If -, was + so max at +
					lda	#$60			
				else
					lda	#-$60			;Else max out at minus
				endif
			endif
			sta	mazvxl		;Update X
		endif
		lda	mazvxl		;Check for possible wall hits
		ifmi				;Moving left
			bit	ltcol,abs
			ifmi				;And Left wall Collision
				lda	#00			;Will stop X left motion
			endif
		else				;Moving Right
			bit	rtcol,abs		;Right Collision??
			ifmi				;yep
				lda	#00			;Stop X Motion
			endif
		endif
		sta	mazvxl		;0 or return mazvxl
		;Y Update
noact		bit	rtcol,abs		;If in frozen counter mode shouldn't hit anything
		ifmi				;Right??
			lda	gamest
			and	#$EF
			sta	gamest		;If do, start up counters again
		endif
		bit	jumpst		;Jumping??
		ifmi				;yes
			lda	objst+zstuf+1
			cmp	#02
			ifeq
				lda	mazvyl
				ifmi
					bit	mzgrnd
					ifmi
						lda	#00
					endif
				endif
				clc
				adc	#02
				cmp	#maxvup
				ifpl
					lda	#maxvup
				endif
				sta	mazvyl	
			else
				lda	#maxvup			;Max Speed Up
				sec
				sbc	mazvyl			;Approach maxvup if jump
				ldx	#03
				jsr	div2x
				sec					;Always add something
				adc	mazvyl
				sta	mazvyl			;mazvyl(new)=mazvyl(ol)+1/32(maxvup-mazvyl(old))
			endif
		else
			bit	mzgrnd		;On Ground??
			ifpl				;No, and not jump, must be falling
				bit	tumble		;If hit head, fall faster (so don't fall up to next floor!!!!)
				ifmi
					lda	mazvyl
					ifpl
						sec
						sbc	#02
						sta	mazvyl
					endif
				endif
				lda	#maxvdn		;Max down velocity
				sec	
				sbc	mazvyl
				ldx	#06
				jsr	div2x
				clc
				adc	mazvyl
				sta	mazvyl		;mazvyl = mazvyl-1/64(maxvdn-mazvyl)
			else
				lda	#00
				sta	mazvyl		;Bring to a stop
			endif
		endif
		;
		;************* Entry from TWShip *****************
rgdr		lda	rgdd			;Slow down read speed
		cmp	#$80
		ror	A
		cmp	#$FF			;Damn, -1 problem not going to 0
		ifeq
			lda	#00
		endif
		sta	rgdd
		rts
		
;***********************************************************
	.sbttl "Man's Left/Right Position"
;***********************************************************
;* Using Man's speed, move man left or right and hold till *
;* proper slow down. If 'scrflg',D7=1 then we are holding  *
;* at one edge location. If D6=1, then left hold           *
;***********************************************************			
m_targx	lda	#02			;Bit 2 indicates just in, wait for ground
		bit	mzgame,abs		;Doing other than maze??
		ifpl
?mp5			rts
		endif
		bne	?mp5			;Wait awhile
		lda	objst+zstuf+2	;Force man motion if in escape pod
		bmi	?mp7
		cmp	#02
		ifcs
			dec	epodfr
			ldy	epodfr		;Need new velocity??
			ifmi
				adc	#00
				cmp	#$0D			;Time to Land??
				ifeq
					lda	#00
					sta	mazvyl
					lda	#$80
					sta	objst+zstuf+2
					;This is needed until table is correct
					lda	#$0B
					sta	objxh+zstuf+2
					lda	#$FC
					sta	objyh+zstuf+2
					lda	#$08
					sta	epodgr
					lda	#snd_d4			;Splash
					jsr	dosound
					lda	#-$28
					sta	mazvxl
					lda	#03
					sta	picseq
					;End stuff needed until table is correct
					bpl	?mp7
				endif
				sta	objst+zstuf+2
				tax
				lda	pode-3,X
				sta	epodfr
			endif
			ldx	objst+zstuf+2
			lda	podx-3,X
			sta	mazvxl
			lda	pody-3,X
			sta	mazvyl
			lda	podg-3,X
			sta	epodgr
		endif
?mp7		ldx	#02			;Will do Y at same time
		begin
			lda	scrflg,X		;Already Holding??
			ifmi				;yes, must look for slow down
				txa				;is X=0
				ifne
					lda	mazvyl-2,X
				else
					lda	mazvxl,X
				endif
				ifmi
					jsr	neg			;Need + Numbers Only
				endif
				cmp	#08
				ifcc
					lda	#00
					sta	scrflg,X		;Re-Center Now
				endif
			else				;Not on Edge, check for possible re-center
				txa				;Which vel to use
				ifne
					lda	mazvyl-2,X		;X is 2 here
				else
					lda	mazvxl,X		;Check for change of direction
				endif
				ifpl					;Guess want Left
					ldy	#$C0
				else
					ldy	#$80			;Else guess Right
					jsr	neg			;+ Velocity
				endif
				cmp	velmax,X		;Velocity to start move
				ifcc				;nope
					ldy	#00			;Will Shut Down
				endif
				tya
				sta	scrflg,X		;Save Flag
			endif
			;**********************************************
			;* Now check flag and add offsets as needed   *
			;* This routine will do all approx scrolling  *
			;**********************************************
			lda	scrflg,X		;Need to Scroll??
			asl	a			;Set C and M from M and V
			ifcs				;yep
				ifmi				;and wish to go left
					lda	xmot+1,X
					cmp	limits,X		;Read limit for this direction ifne
					ifne
						txa				;Is this X
						ifeq				;This is X!
							bit	gamest		;Exit Special
							bvs	?mp15
						endif
						jsr	sub2
					endif
				else				;Want to move right
					lda	xmot+1,X
					cmp	limits+1,X		;Check right limit now
					ifne
						jsr	add2
					endif
				endif
			else				;Wish to move to park postion
				sec	
				lda	xmot,X		;LSB
				sbc	xstopl,X
				sta	temp7			;Save LSB
				lda	xmot+1,X
				sbc	xstop,X		;Stop Value
				ifmi				;At Left(known not 0)
					jsr	add2
				else				;Approach from Right
					ifne
?mp10						jsr	sub2
					else				;Close in to 0
						lda	temp7 		;If it is on the - side, it will go + next time!!
						and	#$F8			;0 (+/-) 7
						bne	?mp10
					endif
				endif
			endif
?mp15			dex
			dex
		miend
		rts
		
podx		.byte $00,$00,$F7,$E6,$E0,$E8,$EE,$EE,$EE,$EC
pody 		.byte $10,$20,$20,$18,$10,$10,$04,$00,$F8,$F8
pode		.byte $10,$50,$24,$2C,$40,$10,$10,$10,$10,$10
podg		.byte $00,$00,$01,$02,$03,$03,$04,$05,$06,$07

;*************************************************
;* Subroutines for above... add and subtract to  *
;* current position of scroll.                   *
;*************************************************
add2		lda	xmot,X
		clc	
		adc	scrolv,X
		sta	xmot,X
		lda	xmot+1,X
		adc	#00
		sta	xmot+1,X
		rts	
		
sub2		lda	xmot,X
		sec	
		sbc	scrolv,X
		sta	xmot,X
		lda	xmot+1,X
		sbc	#00
		sta	xmot+1,X
		rts	
		
;Scroll Velocities..... X=0 for H vel and X=2 for Y vel
scrolv	.byte 6,0,4

;Stop Limits for Motion
xstop		.byte 0,0,-1
xstopl	.byte 0,0,$40

;****************************************************
;* Limits for X and Y scroll (high byte check)      *
;* NOTE: targx will scroll until the high byte      *
;* of the scroll value is equal to the value in     *
;* this table. Therefore, to scroll to -2 the       *
;* value in the table must be -3 to compensate      *
;* for the 2's complement number.                   *
;*                                                  *
;* The values are: xleft, xright, yleft, yright     *
;****************************************************
limits	.byte -2,1,-3,1

;Start Scroll Min speed
	
velmax	.byte $18,$00,$20


;**************************************************
	.sbttl "Maze Scroll Calculations"
;**************************************************
;* This routine moves the man according to his    *
;* velocities as set above. This routine updates  *
;* the 'maze' position, and then calls newpic     *
;**************************************************
posit		lda	tspark
		ifne
			jmp	newpic
		endif
		lda	#00
		sta	temp1
		sta	temp2			;(Y)
		bit	mzgame,abs		;Doing Tube??
		ifvs
			ldy	#00
			lda	mazvyl
			ifmi
				dey
			endif			;Sign Extend 16 bits
			clc
			adc	ymot
			sta	ymot
			tya
			adc	ymot+1
			sta	ymot+1		;Update Y postion
			lda	mazvxl		;Do simple motion here
			clc
			adc	xmot
			sta	xmot
			lda	#00
			adc	xmot+1
			sta	xmot+1		;Move Man
			ifmi
				cmp	#offpos
				beq	?msc10			;Man is holding off screen
			else
				cmp	#02			;Plane Position MSB
				ifcs				;Almost there??
					lda	xmot			;Check for LSB
					cmp	#$60
					ifcs				;There??
						lda	xmot+1		;Man Active??
						cmp	#offpos			;Off screen hold place
						ifne
							lda	#00				;Turn off man
							sta	mazvxl			;Set Speed to 0
							lda	#$10
							sta	frame				;0 Frame from constant wait time
							lda	#offpos			;Store man off screen
							sta	xmot+1			;Hold off screen
							lda	#snd_a2b			
							jsr	dosound			;Start Launch Sound
						else
?msc10						lda	frame
							and	#$3F
							ifeq					;Time to Launch
								lda	#$80
								sta	tstat				;Launch
							endif
						endif
					endif
				endif
			endif
			jmp	bad1
		endif
		lda	mazvxl
		beq	bad1			;Skip if no X Velocity
		cmp	#-1			;Skip -1 too, as it stays here if minus
		beq	bad1
		ifmi				;Moving Left
			bit	ltcol,abs		;Left Colision
			bmi	bad1			;Skip if left collision moving left
		else				;Moving Right
			bit	rtcol,abs		;Right Collision
			bmi	bad1
		endif
		ldx	picseq		;Update Position??
		beq	bad1			;Nope!!
		lda	mazexl
		sta	oldxl			;Save old position
		ldy	#00			;Sign Extend
		lda	velxh			;Add in X Velocity
		ifmi
			clc
			adc	#01			;Must be at least -1
			ifmi				;In case we went back to 0
				dey				;Sign Extend
			endif	
		endif
		ldx	#02
		jsr	div2x			;2X cmp#80 ror
		clc	
		adc	mazexl		;Move X Low
		sta	mazexl		;Move Maze
		tya				;High Byte
		adc	mazexh
		sta	mazexh
bad1		jsr	m_targx		;Offset Routine
		ldy	#00			;Now do Y
		lda	mazvyl		;Add in Y Velocity
		ifmi
			dey				;Sign Extend
		endif
		ldx	#02			;Divide by 2^x
		jsr	div2x
		clc	
		adc	mazeyl
		sta	mazeyl
		tya	
		adc	mazeyh		;Prop to High Byte
		sta	mazeyh
		bit	mzgame,abs		;Which Section
		ifpl
			jsr	newpic
			lda	#$80
			sta	mzgrnd		;Always on ground
			rts
		endif
		ldy	objst+zstuf+2
		cpy	#02
		bcs	?msc20		;Skip if in Pod
		cmp	#$FC			;Top Line Position
		ifcs				;At least on top line
			bit	mzgrnd		;On the ground??
			bmi	?msc15		;If on the ground, skip V checking
			lda	mazeyl		;LSB check
			cmp	#gndv+08		;Above Ground
			ifcs				;Yes
?msc15			lda	gamest		;In Maze Already
				bit	msk20_2			;Already in Maze??
				ifne				;yes
					and	#$DF			;Clear 'already in' bit
					ora	#$40			;Set 'Exit' bit
					sta	gamest		;New Status
					lda	#00
					sta	jbstat
					sta	jumpst		;No more jump
					lda	#snd_hro
					jsr	dosound		;Here Theme
					lda	#$28
					sta	velxh			;Push him toward ship
					lda	velyh
					clc
					adc	#$10
					sta	velyh			;Give him a bit of boost upward
					lda	gamest
					ora	#$10			;Stop Clocks
					sta	gamest
				endif
			else				;Could be falling in from first entry
				cmp	#gndv-08		;At least below line
				ifcc
					lda	gamest
					bit	msk08			;Just Entering??
					ifne				;yep
						and	#$E7			;Start clocks, drop entering bit
						sta	gamest
						lda	#00
						sta	velxh			;Stop X Motion
					endif
				endif
			endif
		else				;Not above so set in flag
			bit	outflg		;In a border stamp??
			ifpl				;no
				lda	objst+zstuf+1	;If man is frozen with boots on, leave him alone
				ifpl
					lda	gamest		;Status
					ora	#$20			;Set Entry Flag, Close Doors
					and	#$BF			;Clear Exit bit
					sta	gamest		;Save new status
				endif
			endif
		endif
?msc20	jsr	newpic		;Place and output picture
		lda	#00
		sta	teeter		;Zero teeter for next frame
		ldx	#zman			;Collision index for the man
		stx	zindex
		jsr	stloc			;Go do Collision routine
		ldy	#zman			;Set for docase
		sty	zindex
		jsr	docase		;Maybe set teeter to 40
		jsr	manact
		;Fall Through to Collisions 
		
;**********************************************
	.sbttl "Man, Object Collision Routine"
;**********************************************
colobj	lda	manstat		;Is man already dead or dying??
		ifeq
?moc1			rts				;Then just leave
		endif
		bmi	?moc1			;He is dying, so leave
		lda	tspark		;Stop collisions if beaming
		bne	?moc1
		lda	#01
		sta	rompg
		ldx	#ztop2-1		;Top of Collision Loop
coloop	stx	zindex		;Save index
		cpx	#zreactor		;Reactor
		beq	bad9			;Special case for Reactor
		cpx	#zspecial		;Skip the ship here
		beq	bad55
		cpx	#zdisc		;Special Objects
		ifcc
			lda	objst,X		;0 if inactive(kept in position)
			bne	?moc3		;Check not exploding if active
			cpx	#ztop
			bcs	bad55
			lda	limbo,X
			beq	bad55			;If not in limbo either, skip it
			pha				;Find which transporter to flash
			and	#07
			tay
			pla
			ifpl
				tya
				eor	#01
				tay
				lda	limbo,X
			endif
			clc
			adc	#08			;Else advance Limbo timer
			ifcs
				lda	stasav,X
				sta	objst,X		;Restore Status
				lda	#00
				beq   ?moc2
			endif
			ifvs
				pha
				lda	#snd_i6
				jsr	dosound
				pla
			endif
?moc2			sta	limbo,X		;In any event, set the active bit for the transporter
			lda	tranhi,Y
			ora	#$80
			sta	tranhi,Y
			jmp	bad5			;Get it back from the transporter after 16 frames
		endif
		lda	objxh,X		;Active??
		beq	bad55			;0 is inactive, so skip it!
?moc3		ifmi				;Is dying, skip this one too
bad55			jmp	bad5
		endif
bad9		lda	#00
		sta	side			;For possible collision bounces
		;Set to Left/Above as initial guess
		lda	mazexl
		cpx	#ztop
		ifcs
			sbc	lsbsx-ztop,X		;X LSB from table in TWMaze
		else
			sec
			sbc	objxl,X		;X LSB
		endif
		sta	temp1			;Save this
		lda	mazexh
		sbc	objxh,X
		sta	temp1+1
		ifmi
			lda	#$80
			sta	side			;Set to Right Side
			jsr	dblneg		;ABS of temp1
		endif
		bne	bad55			;Skip this
		lda	temp1
		cpx	#zcann		;Different collision if laser cannon
		bcc	?moc5			
		cpx	#zcann+nmcann
		ifcc				;Yes, it is a cannon
			lda	canngr-zcann,X
			and	#$0E
			tay
			lda	temp1
			cmp	cannsize,Y
		else
?moc5			cmp	xsize-1,X		;See if we can touch
		endif
		;-1 as no table entry for the man vs. man
		bcs	bad55
		lda	mazeyl
		cpx	#ztrpp		;Trip plates are on floor
		ifcs				;Is a trip plate
			cpx	#zonew		;One way walls are above floor, bypass this
			ifcc
				lda	mazeyh		;Man on same line?
				cmp	objyh,X		;Same line as trip plate?
				bne	?moc10		;No
				lda	ground		;Man on Ground??
				bmi	?moc12		;If on ground we hit it!
				bpl	?moc10
			endif
		endif
		cpx	#ztop
		ifcs				;Specials (Discs or Arrows)
			sbc	lsbsy-ztop,X
		else
			sec
			sbc	objyl,X
		endif
		sta	temp1
		lda	mazeyh
		sbc	objyh,X
		sta	temp1+1
		ifmi
			lda	side
			ora	#$40			;Set to Below hit
			sta	side
			jsr	dblneg
		endif
		ifne
?moc10		jmp	bad5
		endif
		lda	temp1
		cpx	#zcann		;Difference Collision if Laser Cannon
		bcc	?moc11
		cpx	#zcann+nmcann
		ifcc					;Yes it is a cannon
			lda	canngr-zcann,X
			and	#$0E
			tay
			lda	temp1
			cmp	cannsize+1,Y
		else
?moc11		cmp	ysize-1,X		;Check size of object
		endif
		;-1 as no table entry for man vs. man
		bcs	?moc10		;No Collision Here
;*********************************************************
;*****  Collision Occured.. Do Correct Action!!!!!!  *****
;*********************************************************
?moc12	ldy	colact-1,X		;Collision Action Number
		lda	coltbl+1,Y
		pha	
		lda	coltbl,Y
		pha	
		rts	
		
cannsize	.byte $58,$41,$36,$41,$36,$41,$36,$9F,$36,$41,$36,$41,$58,$41

		;Shot Hit
shthit	lda	#$20
		bit	manstat			;Shields on??
		beq	bad11				;No... bye bye baby!
		lda	#snd_i1b			
		jsr	dosound
		dec	shldht
		bpl	bad10				;Had One Left
		lda	#00
		sta	shldok			;Else lose shields, 1 hit gone
		beq	bad10
		
		;Robot or Fireball Hit
rfhit		lda	#00
		sta	rompg
		stx	tempa+1			;I hope it's open!
		lda	#$20
		bit	manstat			;Shields on??
		ifne					;yep!
			lda	#$10
			jsr	bpont2			;And 1000 points
			lda	#01
			sta	rompg
			ldx	tempa+1
			lda	#00
			sta	shldok			;Shields gone now!
			lda	#snd_i1b
			jsr	dosound
		else					;No Shields, he dies!
			lda	#$10
			jsr	bpont2		;And 1000 Points
			lda	#01
			sta	rompg
			ldx	tempa+1
			;Player Dies... Enter at bad11
bad11			lda	#$80
			sta	manstat
			lda	#snd_i2a
			jsr	dosound
			lda	#00
			sta	jumpst		;Clear Jump
			sta	jbstat		;Button Clear also
		endif
		
		;Object Dies... Enter at bad10		
bad10		lda	#$80
		sta	objst,X		;This object will die
bad5		dex	
		ifne
			jmp	coloop		;Continue Loop
		endif
		stx	rompg			;Deselect ROM page 1
		lda	dif4mz
		ifeq
			lda	manstat
			ifne
				jsr	idiotm			;Check to see if idiot message needed
			endif
		endif
		rts	
		
;****************************************************
		.sbttl "Trip Plate Action Routine"
;****************************************************
;* This routine is called from 'colobj' in TWMot    *
;* and is used to set off the fire balls when a     *
;* trip plate is stepped on.                        *
;* Each maze may have up to 8 trip plates. The      *
;* plate index, through the maze number, through    *
;* the 'difcty' level will obtain the trip plate    *
;* table entry.                                     *
;* Each Entry Contains the Following:               *
;* 	XH,YH,XV,YV                                   *
;* Two fireballs will be started (if available)     *
;* at the X,Y location specified. They will be      *
;* offset from each other depending on their        *
;* velocities.                                      *
;*                                                  *
;* Inputs: X=ztrpp+index (to current plate hit)     *
;*                                                  *
;* Note: This routine must not use temp9!!!         *
;****************************************************
trpoint	jsr	chkall		;Make sure this guy didn't fire any
		beq	?tp1			;Already fired
		jsr	getfb			;Any empty??
		ifmi
?tp1			jmp	?tp10			;None Available.. Skip this!
		endif
		sty	tempa			;Save index to fireball
		sta	objyl,Y		;Turn this off
		sta	objxl,Y
		lda	#snd_i2d
		jsr	dosound
		lda	mazx4
		asl	a
		sta	tempa+1
		txa	
		sec	
		sbc	#ztrpp		;Trip point 0-7
		clc	
		adc	tempa+1		;Trip # + (maznum*8)
		sta	tempa+1
		asl	a
		clc	
		adc	tempa+1		;((maznum*8)+trp #)*3
		tay				;As indirect index
		ldx	tempa			;Get Index
		lda	(trind,Y)		;Get XH
		pha				;Save for possible flag
		and	#$1F			;Drop Flags
		sta	objxh,X
		sta	temp4+1		;Save for possible other
		iny	
		lda	(trind,Y)
		sta	objyh,X		;Set Y Position
		sta	temp5+1		;Save for possible other
		iny	
		lda	(trind,Y)		;X vel
		sta	velxh,X
		sta	temp6			;For Possible other
		ifpl
			lda	#$80
		else
			lda	#$7F
		endif
		sta	objxl,X
		sta	temp4			;Save for possible other
		lda	incdif		;0-4
		asl	a
		asl	a
		asl	a
		clc	
		adc	temp6
		and	#$7F			;0 - 27
		tay	
		lda	ispeed,Y
		bit	temp6			;Add back in direction
		ifmi
			jsr	neg
		endif
		sta	temp6
		sta	velxh,X
		lda	#00			;Always 0
		sta	velyh,X
		sta	temp6+1		;Save for possible other
		ifeq				;If Y vel 0, then space Y
			lda	#$48
			sta	objyl,X
			lda	#$D7
			sta	temp5			;Space Y apart
		endif
		lda	temp3			;The saved status from chkall
		sta	objst,X
		pla				;Recall Flag(s)
		ifpl				;Need two
			jsr	getfb			;Get us another one
			bmi	?tp10			;Sorry None available
			lda	temp4			;XLSB from above
			sta	objxl,Y
			lda	temp4+1
			sta	objxh,Y		;XMSB from above
			lda	temp5
			sta	objyl,Y		;YLSB from above
			lda	temp5+1
			sta	objyh,Y		;YMSB from above
			lda	temp6
			sta	velxh,Y		;Velocity from above
			lda	temp6+1
			sta	velyh,Y	
			lda	temp3			;Saved status
			sta	objst,Y		;Turn on other too
		endif
?tp10		ldx	zindex		;Restore X
		jmp	bad5			;And Continue
		
;********* Make sure this trip pad does not have a fire ball out there ********* 
chkall	txa	
		sec	
		sbc	#ztrpp		;Just the index of this pad
		ora	#$10			;So compare works with status bit
		sta	temp3			;Save for store in Main Routine
		;A=index of this trip point ! 10(for status on bit)
		ldy	#07
		begin
			lda	objst+zfire+8,Y		;Look at top 8
			and	#$BF				;Drop Screen Bit
			cmp	temp3				;Same as out??
			beq	?ca10				;Shit... found one
			dey
		miend
		;Returns with - flag, bu A=what we shall store to status
		rts	
?ca10		ldy	#00
		rts				;0 is bad return
		
;********* Get Fireball ************
getfb		ldy	#zfire+nmfire-1		;Look at top 8
		begin
			lda	objst,Y
			ora	limbo,Y
			beq	?gf10
			dey	
			cpy	#zfire+8
		ccend				;None available
		lda	#$80
?gf10		rts

;********* Adds 1 in Decimal to Disc Count ********* 	
updiscs	clc	
		lda	tottim
		adc	#$10			;Add 16 counts
		ifcs
			lda	#$FF			;Don't allow to wrap
		endif
		sta	tottim
		lda	#snd_i2g
		jsr	dosound		;Sound for it!
		lda	nxtdisc		;Points for this one
		bit	objst+zreactor	;Reactor set off??
		ifmi
			sed				;*********** Decimal Mode **************	
			lda	nxtdisc
			clc	
			adc	#02
			sta	nxtdisc
			cld				;*********** End Decimal  **************
		endif
		ldx	#00
		stx	rompg			;Set to page 0 for a bit
		jsr	bpoint
		lda	#01
		sta	rompg			;Reset to page 1 for rest of collisions
		ldx	zindex		;Restore X
		lda	#$80
		sta	objxh,X		;Kill this one
		jmp	bad5

;********* Hit Reactor ************
reahit	lda	objst,X		;Already Set off?
		ifpl				;Not yet
			lda	objyh+zreactor		;Make sure man and reactor are on same line
			cmp	objyh+zman
			ifne
				jmp	bad5		;Not on same line, skip collision
			endif
			lda	reacst		;Reactor time left
			and	#$7F
			sta	retime
			lda	#$FF
			sta	retime+1
			lda	#$50			;5000 for reactor (pass BCD)
			ldx	#$08
			stx	nxtdisc		;So next one will be worth 1000 (see above)
			ldx	#00
			stx	lauen			;Ready for Launch
			stx	rompg			;Set to page 0 for a bit
			bit	reacst
			ifpl
				jsr	bpoint		;Don't call each time you hit the reactor
			endif
			lda	#01
			sta	rompg			;Reset to page 1
			jsr	newarrow		;Out Maze arrows
			lda	#$E4
			sta	retbuf+2		;Turn on Clock
			ldx	zindex		;Recall old X
		endif
		lda	#$80
		sta	rtcol,abs		;Don't walk through it!
		ldx	zindex
		jmp	bad10
		
;********* Vertical Lightning Hit ************
dofrfl	lda	mazeyh
		cmp	objyh,X		;Make sure same line
		ifeq
		;********* Horizontal Lightning Hit ************
		;********* Laser Cannon Collision   ************
docann		lda	objst+zstuf+2
			cmp	#02			;Skip collisions if in escape pod
			bcs	?lh10			
			lda	#snd_i2a
			jsr	dosound
			lda	#$80
			sta	manstat		;Do this here, can't jump to bad11
			lda	#00
			sta	jumpst		;it will try and kill the lightning too
			sta	jbstat		;and that clobbers a robot velocity
		endif
?lh10		jmp	bad5			;Next!!!

;********* One Way Walls ************
doonew	lda	mazeyh
		cmp	objyh,X		;Make sure same line
		ifeq
			ldy	onewst-zonew,X	;Status of this wall: - One way to left... + One way to Right
			lda	side
			ifmi
				tya				;See if this is one way right
				ifmi				;if -, no right allowed
					lda	#$80
					sta	rtcol,abs
				endif
			else
				tya				;See if one way left
				ifpl				;If +, no left allowed
					lda	#$80
					sta	ltcol,abs
				endif
			endif
		endif
		jmp	bad5			;Next!!!!
		
pick		lda	objst,X		;Pick up an object
		cmp	#01
		ifeq
			inc	objst,X		;Activated
		endif
		rts	
		
dstop		lda	#$80			;Stop man from moving through and object
		ldy	side
		ifmi
			sta	rtcol,abs
		else
			sta	ltcol,abs
		endif
		rts	
		
dunder	lda	objyh,X		;Check if same height MSB as man
		cmp	objyh
		rts	
		
doclock	jsr	dunder		;Check is on same level
		bne	?dc10
		jsr	pick			;Pick it up
		jsr	dstop			;But don't let him through it
?dc10		jmp	bad5

doboot	jsr	dunder		;Check if on same level
		bne	?db10		
		lda	objst+zstuf+1	;Pick it up
		cmp	#01
		ifeq
			lda	#$80
			sta	objst+zstuf+1
			lda	#$D0			;Freeze Timers, Cut out controls
			sta	gamest
			lda	#00			;Zero Velocity
			sta	velxh
			sta	velyh
		endif
?db10		jmp	bad5

dotite	lda	mazvyl			;Do Stalactite
		ifpl
			cmp	#$10
			ifcs
				jsr	hd12			;Hit Head
			endif
		endif
		lda	mazvxl
		jsr	lf12				;Check if horizontal velocity high
		lda	#00
		sta	jumpst			;Make him fall down in any event
		sta	mazvxl			;Also stop his sideways progress
		bit	tumble
		ifmi
			lda	#snd_i2f
			jsr	dosound
			lda	#02				;Ring it like a bell
			sta	objst,X			;I'll believe X is correct when I see it!!
		endif
		jmp	bad5				;Next Object Please
		
dolock	jsr	dstop				;Don't let him move through it
		lda	objst+nmkeys,X		;Look at corresponding key
		and	#$10
		ifne					;He has it
			lda	#00
			sta	objst+nmkeys,X		;Take away the key
			sta	objst,X			;Take away the lock
			lda	#snd_i2i
			jsr	dosound
		else
			jsr	lf11
		endif
		jmp	bad5				;Next Object...
		
dokeys	jsr	dunder			;Check to see if on same level
		bne	?dk10
		lda	objst,X
		cmp	#$10
		ifcc
			lda	objst,X
			ora	#$10
			sta	objst,X			;He picked it up
			lda	#snd_i2h
			jsr	dosound
			stx	temp4
			lda	#$10				;1000 Points
			jsr	bpont2
			ldx	temp4
		endif
?dk10		jmp	bad5

dopod		lda	objst+zreactor		;Escape Pod
		bpl	?dp10				;Nothing happens if reactor not blowing up
		lda	objst+zstuf+2
		cmp	#01
		bne	?dp10				;If already set, do nothing
		lda	#snd_a2b2			;Take off sound
		jsr	dosound
		jsr	dodelay
		lda	#snd_j6
		jsr	dosound
		lda	#00				;Set man to position of pod without screen jump
		tay	
		sec	
		sbc	objxl
		sta	temp1
		lda	objxh+zstuf+2
		sbc	objxh
		sta	temp1+1			;Man's position change must be added to xmot
		jsr	spod				;Y is 0
		lda	#$80				;Now handle Y coordinate change
		sec	
		sbc	objyl
		sta	temp1
		lda	objyh+zstuf+2
		sbc	objyh
		sta	temp1+1			;Man's position change must be added to xmot
		ldy	#02
		sty	objst+zstuf+2
		jsr	spod				;2 in Y
		lda	#00
		sta	objxl
		sta	objst+zstuf+1		;Turn off Boots (just in case)
		sta	jbstat
		sta	jumpst
		sta	rtcol,abs			;*** Make sure Oxygen count stops!! ****
		;Above is because a collision with right wall in pod restarts oxygen time
		lda	#$80
		sta	objyl
		lda	objxh+zstuf+2
		sta	objxh
		lda	objyh+zstuf+2
		sta	objyh
		lda	#$D0
		sta	gamest			;Run him off toward large ship
?dp10		jmp	bad5				;Next Object....

spod		lda	xmot,Y			;Support routine for dopod
		clc	
		adc	temp1				;Y is 0 or 2
		sta	xmot,Y
		lda	xmot+1,Y
		adc	temp1+1
		sta	xmot+1,Y
		rts	

dohand	bit	ground			;Do De Hand
		ifmi					;Can't hit switchbox unless on the ground
			lda	#01
			ldy	mazvxl
			beq	?dh10
			ifmi
				lda	#03			;Make it retract
			endif
			cmp	objst+zstuf+3	;Already in this state?
			ifne
				sta	objst+zstuf+3	;Store the new state
				cmp	#01			;Turning on??
				ifeq
					lda	#snd_i4c		;Sound On
				else_ne
					lda	#snd_i4b		;Sound 'off'
				endif
				jsr	dosound
				lda	#$18
				sta	sndcue+1
				sta 	sndcue+2
			endif
			;No need to store it away if already the same!!
		endif
?dh10		jmp	bad5				;Next please!!

dotran	lda	tspark			;Not currently transporting
		ifeq
			lda	objst,X
			and	#$10
			ifeq					;Enter from Left
				lda	objxl
				ifpl					;Moving to Right
					lda	mazvxl
					ifpl
?dt10						lda	objxl
						ifmi
							jsr	neg
						endif
						cmp	#$60
						ifcs
							txa				;Entered transporter
							eor	#01			;Find it's partner
							tay
							lda	objst,Y
							ora	#$20
							sta	objst,Y		;Booth to travel to
							lda	#01
							sta	tspark		;Start Phase out effect
							lda	#snd_i6
							jsr	dosound
						endif
					endif
				else					;Moving to Left
?dt20					jsr	dstop				;Hit side
					jsr	lf11				;Maybe hit hard
					lda	objxl
					ifpl
						lda	#$24
					else
						lda	#$DC
					endif
					sta	objxl
				endif
			else					;Enter from Right
				lda	objxl
				bmi	?dt10				;Hit Transporter moving to right
				lda	mazvxl
				bpl	?dt20
			endif
		endif
		jmp	bad5
		
;*****************************************
    .sbttl "Size Tables"
;*****************************************   
tmpptr      = $
o_objsz     = 0
nmcollobj   = (ztop2-zreactor)
xsize       .block nmcollobj
ysize       .block nmcollobj

#define SETCOLL(cnum,xval,yval)  \tmpptr .set *
#defcont                        \ .org xsize+o_objsz
#defcont                        \ .fill cnum,xval
#defcont                        \ .org ysize+o_objsz
#defcont                        \ .fill cnum,yval
#defcont                        \o_objsz .set o_objsz+cnum
#defcont                        \ .org tmpptr
#defcont                        \#IF o_objsz > (nmcollobj)
#defcont			            \	.error "RPM: Too collision objects defined. Increase limit or fix."
#defcont				        \#ENDIF 
   
        SETCOLL(nmreactor,$60,$60)  
        SETCOLL(nmfire,$28,$48)
        SETCOLL(nmlsht,$28,$48)
        SETCOLL(nmcann,$60,$40)
        SETCOLL(nmrob,$28,$48)
        SETCOLL(nmshot,$18,$40)
        SETCOLL(1,$10,$40)          ;Rex's Ship on top of Maze
        SETCOLL(nmtite,$58,$58)
        SETCOLL(nmtran,$5C,$80)
        SETCOLL(nmlock,$30,$80)
        SETCOLL(nmkeys,$34,$68)
        SETCOLL(1,$54,$60)          ;Clock
        SETCOLL(1,$30,$60)          ;Boots
        SETCOLL(1,$80,$80)          ;Escape Pod
        SETCOLL(1,$20,$50)          ;De Hand
        SETCOLL(nmdisc,$30,$3F)     ;Oxygen
        SETCOLL(nmligh,$80,$30)     ;Lightning (Horizontal)
        SETCOLL(nmfrfl,$30,$98)     ;Force Fields (Vertical)
        SETCOLL(nmtrpp,$88,$88)     ;Trip Pads
        SETCOLL(nmonew,$40,$98)     ;One Way Walls
                
;************************************************
;* These are the LSB positions for non-motion 
;* objects where their maze stamp is their 
;* locations. These LSB's position within
;* the stamp.    
;************************************************
o_lsbs  = 0
nmstatobj   = (ztop2+nmarow-ztop)
;nmstatobj = (ztop2-ztop)
lsbs        .block nmstatobj
lsbsy       .block nmstatobj

#define SETLSB(lnum,xval,yval)   \tmpptr .set *
#defcont                        \ .org lsbs+o_lsbs
#defcont                        \ .fill lnum,xval
#defcont                        \ .org lsbsy+o_lsbs
#defcont                        \ .fill lnum,yval
#defcont                        \o_lsbs .set o_lsbs+lnum
#defcont                        \ .org tmpptr
#defcont                        \#IF o_lsbs > (nmstatobj)
#defcont			            \	.error "RPM: Too static object LSBs defined. Increase limit or fix."
#defcont				        \#ENDIF 

    ;SETLSB(number,xlsb,ylsb)
    SETLSB(nmtite,$80,$B0)
    SETLSB(nmtran,$80,$80)
    SETLSB(nmlock,$80,$80)
    SETLSB(nmkeys,$00,$40)
    SETLSB(1,$00,$40)       ;Clock
    SETLSB(1,$00,$34)       ;Boots
    SETLSB(1,$00,$80)       ;Escape Pod
    SETLSB(1,$3C,$01)       ;De Hand
    SETLSB(nmdisc,$90,$40)  ;Oxygen
    SETLSB(nmligh,$00,$80)  ;Lightning (Horizontal)
    SETLSB(nmfrfl,$80,$80)  ;Force Field (Vertical)
    SETLSB(nmtrpp,$80,$08)  ;Trip Pads
    SETLSB(nmonew,$80,$80)  ;One Way 
    SETLSB(nmarow,$C0,$40)
        

;Collision Action Table
colact      .byte $00,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$14,$14,$14,$14,$02,$02,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$04,$04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$10,$16,$16,$16,$16,$16,$18,$18,$18,$18,$18,$18,$18,$18,$1A
		.byte $1A,$1A,$1A,$1C,$1C,$1C,$1C,$1E,$0E,$20,$22,$06,$06,$06,$06,$06
		.byte $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$08,$08,$08,$08,$08
		.byte $08,$08,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0C,$0C,$0C,$0C,$0C,$0C,$0C
		.byte $0C,$12,$12,$12,$12

;Address of routines based on index from above		
coltbl	.word reahit-1, rfhit-1,  shthit-1,  updiscs-1
		.word docann-1, dofrfl-1, trpoint-1, doboot-1
		.word bad5-1,   doonew-1, docann-1,  dotite-1
		.word dotran-1, dolock-1, dokeys-1,  doclock-1
		.word dopod-1,  dohand-1

;Unit pointer offset tables
maz0u		.byte $00,$03,$06,$09,$0C,$0F,$12,$22,$32,$42,$52,$62,$72,$82
maz1u		.byte $00,$03,$06,$09,$0C,$0F,$12,$27,$3C,$51,$66,$7B,$90,$A5,$BA
maz2u		.byte $00,$03,$06,$09,$0C,$0F,$12,$27,$3C,$51,$66,$7B,$90,$A5,$BA,$CF
maz3u		.byte $00,$03,$06,$09,$0C,$0F,$12,$25,$38,$4B,$5E,$71,$84,$97,$AA,$BD,$D0,$E3
		
;Unit Pointers - Point to the table of unit pointers for each maze
mazunt	.word	maz0u, maz1u, maz2u, maz3u 

;*******************************************
	.sbttl "Get Pointer"
;*******************************************
unitp
getptr	lda	maznum
		asl	a
		tay				;Offset into unit pointer offset table
		lda	mazunt+1,Y		;Pointers to offsets
		sta	mazpt+1
		lda	mazunt,Y
		sta	mazpt
		lda	linen			;Line Number
		clc	
		adc	mazpt			;Add Line to unit address
		sta	mazpt
		ifcs
			inc	mazpt+1
		endif			;Now points to offset into data for this line
		ldy	#00
		lda	(mazpt,Y)
		clc	
		adc	#(mazer&$ff)	;Add to low byte
		sta	mazpt
		lda	#00
		adc	#(mazer/$100)&$ff	;Prop carry to high byte
		sta	mazpt+1		;Now points to data for this line
		rts	

;*******************************************
	.sbttl "Idiot Message Routines"
;*******************************************		
idiotm	bit	jumpst
		ifmi
			bit	jumprv
			ifpl
				lda	objyh
				asl	a
				asl	a
				asl	a
				asl	a
				ora	objxh			;Compact MSB location to one byte
				ldx	#estmps-jstmps-1
				begin
					cmp	jstmps,X
					beq	?im10
					dex
				miend
				lda	#00			;Not on one of the idiot stamp
				sta	jmptim
				beq	?im20			;bra
?im10				lda	#01
				sta	jmptim		;Starting jump in correct stamp
			else
				lda	jmptim		;Jump in progress??
				ifne
					cmp	#$18
					ifcc
						clc
						adc	#01
					endif
					sta	jmptim
				endif
			endif
		else
			bit	jumprv
			ifmi				;Starting to fall
				bit	tumble
				ifpl
					lda	jmptim
					ifne
						cmp	#$18
						ifne
							inc	fldcnt
							lda	fldcnt
							cmp	#03
							ifeq
								lda	#00
								sta	fldcnt
								lda	#01
								sta	mestim
							endif
						endif
					endif
				endif
			endif
		endif
?im20		lda	jumpst
		sta	jumprv
		lda	mestim
		ifne
			clc
			adc	#01
			cmp	#$80
			ifcs
				lda	#00
			endif
		endif
		sta	mestim
		rts
			
idiotd	lda	#00			;Display the "hold button for higher jumps" message
		ldx	#$70
		jsr	vgadd2
		lda	#$F0
		ldx	#$FE
		jsr	vgvtr5
		ldx	#mpub0
		stx	temp4
		jsr	msgnop
		lda	#$B0
		ldx	#$F6
		jsr	vgvtr5
		ldx	#mpub1
		stx	temp4
		jsr	msgnop
		lda	#$AF
		ldx	#$20
		jmp	vgvtr5
		
jstmps	.byte $A3,$A4,$A5,$A6,$A7,$A8,$96,$97,$88,$89,$8A,$79,$7A
estmps

;****************************************************
;*     Collision Routines!!!!!                      *
;****************************************************
	.sbttl "Stamp Locator"
;****************************************
;* Determine which stamp the guy is in  *
;* and also save his position in that   *
;* stamp for possible later use.        *
;*                                      *
;* Input:	(X)=Offsets into positions  *
;*              obj*l and obj*h         *
;*                                      *
;* Uses: 	temp3,temp4,X		    *
;****************************************
stloc		lda	objxl,X
		sta	temp3				;Location of this stamp
		lda	objxh,X			;Stamp Position
		sta	temp3+1
		lda	objyl,X
		sta	temp4				;Place in this stamp
		lda	objyh,X
		sta	temp4+1			;Line On
		jsr	sttype			;Set current stamp type
		sta	curstmp			;Current Stamp in
		inc	temp4+1
		jsr	sttype
		and	#$0F				;Don't need flags here
		sta	abvstmp			;Stamp above my head
		dec	temp4+1
		;Fall through to ground and head check

;****************************************
	.sbttl "Ground Check"
;****************************************
;* Enter from above to set proper 0 page*
;* Variables and X must point to posit  *
;*                                      *
;* Ground check.... If mazeyl = 50 then *
;* then there is a possible ground      *
;* collision. Will then have to check   *
;* X position to see what is there.     *
;*                                      *
;* Also, if = -50 then possible head    *
;* collision, Make X check for roof     *
;*                                      *
;* Exit: (A)= tgrnd value               *
;****************************************		
grchk		lda	#00			;Clear Just past bit
		tay				;Guess no head collision
		sta	abvg,abs			;Not above ground as a guess
		sta	pastbit
		sta	undg,abs		;Also guess above ground
		lda	temp4			;mazyl from above
		ifpl				;Want to check ground
			cmp	#$10			;At least above the stamp
			ifcs
				sec
				sbc	gndvt,X		;Ground Table
				ifmi				;Might have gone by it
					lda	velyh,X		;Moving down
					ifmi
						lda	temp4			;New Position
						sec
						sbc	velyh,X		;Subtract back in vel and check
						cmp	gndvt,X
						ifcs				;Yep, it went by
							lda	#$80
							sta	pastbit		;Signal it just went past
						else				;Otherwise, we are underground
							lda	#$80
							sta	undg,abs			;Set this for later
							lda	#00
						endif
					else			;Underground moving up!!
						lda	#$80
						sta	undg,abs	;Set this for later
						lda	#00
					endif
				else				;Not past, just above?
					cmp	#04			;Slope amount
					ifcc				;On ground
						lda	#$80
					else
						lda	#00
					endif
				endif
			else				;Not near ground
				lda	#$80
				sta	undg,abs		;But for sure under it!
				lda	#00
			endif
		else				;Not near ground, near ceiling??
			sec	
			sbc	celt,X		;Can he hit his head??
			ifmi				;Is below, but is T close?
				cmp	#-4			;How close?
				ifcs				;Close enough
					ldy	#$80			;Will hit his head!
				endif
			else
				lda	#$80			;Is above, did it just pass?
				sta	abvg,abs		;His head is above ceiling
				lda	temp4
				sec
				sbc	velyh,X		;Back out velocity
				cmp	celt,X		;Did it pass this time?
				ifcc
					ldy	#$80
				endif
			endif
			txa				;X=0 for man
			bne	?gc10			;Only skip for man
			lda	#02
			and	mzgame,abs		;Waiting for game to start
			ifeq
?gc10				tya
				sta	headcol,X		;Possible Head collision
			endif
			lda	#00
		endif
		sta	tgrnd
		lda	ground,X			;Save if change state
		sta	lastgnd,X
		rts	
		
gndvt		.byte gndv		;Man's Ground Value
		.byte $40		;Reactor's Ground Value
		.byte $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28		;FireBalls
		.byte $28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28,$28
		.byte $10,$10,$10,$10							;Cannon
		.byte $18,$18,$18,$18,$18,$18,$18,$18,$18,$18			;Robots
		.byte gndv,gndv,gndv,gndv,gndv,gndv,gndv,gndv,gndv,gndv	;Shots
	
celt		.byte celing	;Man's Ceiling
		.byte $C8		;Reactor
		.byte $D8,$D8,$D8,$D8,$D8,$D8,$D8,$D8,$D8,$D8,$D8,$D8		;Fireballs
		.byte $D8,$D8,$D8,$D8,$D8,$D8,$D8,$D8,$D8,$D8,$D8,$D8
		.byte $F0,$F0,$F0,$F0							;Cannon
		.byte $D8,$D8,$D8,$D8,$D8,$D8,$D8,$D8,$D8,$D8			;Robots
		.byte $E8,$E8,$E8,$E8,$E8,$E8,$E8,$E8,$E8,$E8			;Shots

;****************************************
	.sbttl "Man's Action on Collisions"
;****************************************
;* This routine is called after calling *
;* stloc and grchk to effect the mans   *
;* velocities and picture sequences     *
;* based on bits set by those routines  *
;****************************************	
manact	lda	curstmp
		and	#$0F			;Only stamp info
		sta	manstmp		;Save this for later use
		bit	curstmp	
		ifvs
			lda	#$80			;An outside stamp
		else
			lda	#00
		endif
		sta	outflg		;Set outflg
		bit	mzgrnd
		ifmi				;On ground??
			bit	mzltgd		;Just Land?
			ifpl				;yep
				lda	picseq		;Only if at stop
				and	#08			;Was falling??
msk08 = $-1							;A bit #08 mask!!!
				ifne				;Was a fall sequence
					lda	picseq
					and	#02			;Look for jog or run
					ifeq				;Was stop of walk
						ldy	#04			;Don't change pics to fast(stored below)
						lda	#$80
						sta	landflg		;Landing
						lda	#landseq
						sta	picseq		;Set to land sequence
						lda	#$10			;Set to squat pic to start bounce seq
						bit	tumble		;Only if tumbling
						ifmi
							lda	#02			;So sit there for a while
							sta	sittime		
							bit	face			;Smash face??
							ifpl				;no, do normal stuff for hit head
								lda	#13d			;force to squat
								ldy	#08			;and some time til next sequence
							else
								lda	piccur		;Else, continue sequence in table
								ldy	#06			;Do it slowly
							endif
						endif
						sta	piccur		;Start new sequence
						sty	picdely
						lda	#$80
						sta	stopflg		;To indicate a change to stop sequence
					endif
				endif
				lda	#$80
				sta	mzgame,abs		;This may clear any hold bit left on
			endif
		endif
		lda	mzgrnd
		and	pastbit			;On Ground and just past it?
		ifmi
			sec
			lda	#gndv
			sbc	temp4			;Amount under 50
			clc
			adc	mazeyl		;Add back to position
			sta	mazeyl		;Hold on the ground
		endif
		jsr	docse2		;Do head check also and return
lf11		lda	mazvxl		;But was he moving very fast
		ifpl
			bit	rtcol,abs
			bmi	lf13
		else
			bit	ltcol,abs
lf12			ifmi
				jsr	neg		;Look at + velocity
lf13				cmp	#$50		;At least running??
				ifcs			;yep
					lda	#00
					sta	jumpst		;Stop any jump also
					jsr	hd12			;Tumble down
					bit	mzgrnd		;Did he do it on the ground
					ifmi				;yep
						lda	#35d			;For hitting a wall
						sta	piccur		;Smack the wall face first
						lda	#$0C			;Any + value will do
						sta	mzltgd		;Fool it next time and say just landed
						sta   mzgrnd		;A bit more fooling
						sta	picdely
						lda	#$80
						sta	face			;Tell it we smashed our face
					endif
				endif
			endif
		endif
		rts

;****************************************
	.sbttl "Stamp Lookup Routine"
;****************************************
;* Uses info from stamp locator and     *
;* returns the type of the stamp        *
;* that he is currently standing in.    *
;* Expects that X & Y position info     *
;* will be in temp3(XL & XH) and        *
;* temp4 (YL & YH )                     *
;*                                      *
;* Uses: temp5,temp1,temp3,temp4        *
;****************************************				
sttype	
;********** Out of bounds check first **********
		lda	temp4+1		;Y MSB (above maze check)
		cmp	#$FD			;This is above top line
		ifcs
?sl10			lda	#$47			;Always blank and outside
			rts
		endif
		lda	temp3+1		;X MSB (Left of maze)
		beq	?sl10
		bmi	?sl10
		sty	temp7			;Save Y
		lda	#02			;Fudge factor that works always
		sec	
		sbc	temp4+1		;Y Position (negated)
		sta	linen			;This line for stamp
		jsr	unitp			;Get unit pointers
		lda	mazpt
		clc	
		adc	temp3+1		;Current stamp in this line
		sta	mazpt
		ifcs
			inc	mazpt+1
		endif
		clc	
		adc	#03			;Another weird offset factor
		sta	mazpt
		ifcs
			inc	mazpt+1
		endif
		lda	(mazpt,Y)		;Get stamp
		ldy	temp7
		rts	
		
;****************************************
	.sbttl "Ground Case Table"
;****************************************
;* Collision case routine for ground    *
;* collisions. Requires 'curstmp' to    *
;* contain current stamp type the man   *
;* is standing in. Also requires that   *
;* temp3 & temp4 contain X and Y        *
;* positions.                           *
;* Will return as a subroutine          *
;*                                      *
;* Caution: (Y) = Object Index          *
;*          (Also in zindex)            *
;****************************************	
coljsr	.word stamp0-1
		.word stamp1-1		;Stamp 1 (H Line)
		.word stamp2-1		;Stamp 2 (H end, right down)
		.word stamp3-1		;Stamp 3 (H end, right up)
		.word stamp4-1		;Stamp 4 (H end, left up)
		.word stamp5-1		;Stamp 5 (H end, left down)
		.word stamp6-1		;Stamp 6 (V Line)
		.word stamp7-1		;Stamp 7 (Black)
coljse 	;end of table marker

docase	lda	#00
		ldy	zindex		;0 if doing man
		sta	rtcol,Y
		sta	ltcol,Y
		lda	curstmp		;Current Stamp
		and	#$0F
		asl	a			;Word entry to above
		tax				;into X
		cpx	#coljse-coljsr	;Valid stamp?
		ifcs				;not valid
			ldx	#02			;Set to line for now
		endif
		lda	coljsr+1,X
		pha	
		lda	coljsr,X
		pha	
		rts				;Do case (jmp equiv)
		
;****************************************
	.sbttl "Stamp calls for Ground"
;****************************************
;* Stamp 0 - No Stamp 0, this is an     *
;* end of line marker, should not ever  *
;* happen                               *
;****************************************	
stamp0	rts			;No action!

;****************************************
;* Stamp 1 - H line, set ground bit if  *
;* at ground level always               *
;****************************************	
stamp1	lda	tgrnd
		sta	ground,Y	;Ok to stop here
		rts
		
;****************************************
;* Stamp 2 - H Line, Right edge down.   *
;* If H position is greater than 80     *
;* then will clear ground bit, else     *
;* use tgrnd from above.                *
;****************************************		
stamp2	lda	tgrnd
		ldx	temp3			;X LSB from way above
		ifmi
			cpx	#lftwal-$18		;On Edge, time to fall::
			ifcs
				jsr	setlft		;Set at left edge
				lda	#00
			endif
		endif
		sta	ground,Y
		tax				;Set Zero Flag
		ifeq				;If not on ground
			bit	undg,abs		;Under Ground??
			ifmi
				jsr	rtchk			;See if skewered
				jmp	stamp634			;Don't allow to pass wall
			endif
		endif
		rts	

;****************************************
;* Stamp 3 - H Line, Right edge up.     *
;* Should be no way to pass this edge   *
;* if X position > edge. set rtcol      *
;* if on ground                         *
;****************************************		
stamp3	ldx	temp3			;LSB X position
		ifpl
			lda	tgrnd
		else
			lda	#00
		endif
		sta	ground,Y
		jsr	stamp634			;Do wall check as vertical wall
		bit	undg,abs		;Under Ground Level??
		ifmi				;yes
			jmp	rtchk			;check for possible skewer
		endif
		rts
		
;****************************************
;* Stamp 4 - H Line, Left edge up.      *
;* Should be no way to pass this edge.  *
;* Set ltcol if at or passed this edge  *
;****************************************				
stamp4	ldx	temp3			;LSB X from way above
		ifmi				;On Right edge
			lda	tgrnd			;Get ground status
		else
			lda	#00
		endif
		sta	ground,Y
		jsr	stamp634			;Do wall check as vertical wall
		bit	undg,abs			;Under ground level??
		ifmi				;yes
			jmp	ltchk			;check left for possible skewer
		endif
		rts

;****************************************
;* Stamp 5 - H Line, Left edge down.    *
;* If over left edge, will clear ground *
;* Else will use tgrnd from above       *
;****************************************				
stamp5	lda	tgrnd
		ldx	temp3			;LSB X position from above
		ifpl				;On ledge??
			cpx	#rtwal+$10		;yep, time to fall??
			ifcc
				jsr	setrt			;Set right wall
				lda	#00
			endif
		endif
		sta	ground,Y
		tax				;Test A for 0 (possibly not set!)
		ifeq
			bit	undg,abs		;Under ground??
			ifmi
				jsr	ltchk			;check for skewer
				jmp	stamp634
			endif
		endif
		rts

;****************************************
;* Stamp 6 - Vertical Wall              *
;* Do not allow to pass if hitting wall *
;* may pass if above of below if room.  *
;* Ground is always cleared unless      *
;* standing on edge of wall.            *
;****************************************		
stamp6	jsr	stamp7		;Do check for side stamps
stamp634	ldx	abvstmp		;Whats above??
		lda	abvchk,X		;Can we pass this on
		bmi	bad7			;No need to check here
		lda	temp4			;Y LSB
		cmp	#gndv+$75		;Top of this wall here
		;At this point it is almost in the next stamp!!!!!!
		ifcs				;We are above
			cmp	#gndv+$80		;May need to fall a bit
			ifcs
				jmp	bad12			;No ground here!!
			endif
			lda	temp3
			cmp	#rtwal+$10
			ifcs
				cmp	#lftwal-$18
				ifcc				;We are standing on top!
					tya
					ifeq
						lda	#$40
						sta	teeter		;Teetering
					endif
					lda	#$80
					sta	ground,Y
				else				;Else, falling, make sure outside edge
					jsr	setlft
				endif
			else				;Else falling, make sure outside edge
				jsr	setrt
			endif
		else				;Not above wall
bad7			ldx	#$80
			;check for possible wall pass through
			lda	velxh,Y		;Left check left, right check right
			ifmi
				lda	temp3			;X LSB
				cmp	#$20			;Max velocity and stop stamp crossover
				ifcs				;no need to check from 20 to 0
					cmp	lcolv,Y		;At left wall (size include)
					ifcc				;We know we are left, did we just pass through??
						lda	oldxl,Y		;See if old pos was right of wall
						bmi	?stm10		;Go set collision left wall
					endif
				endif
			else			;Moving right, check right wall
				lda	temp3		;XLSB from motion routine
				cmp	#-$20		
				ifcc			;Same as above for other wall
					cmp	rcolv,Y	;At right wall (size table)
					ifcs				;We know we are right of right wall, did we just pass through??
						lda	oldxl,Y		;See if old pos was left of wall
						bpl	?stm15		;Go Set collision
					endif
				endif
			endif
			lda	temp3			;X LSB
			ifmi				;Could approach from left
				cmp	lcolv,Y		;At left wall (size include)
				ifcc
?stm10				lda	lcolv,Y		;Set at left wall (slightly in)
					sec
					sbc	#01
					sta	objxl,Y
					txa
					sta	ltcol,Y		;Stop left motion
				endif
			else
				cmp	rcolv,Y			;At right wall (size table)
				ifcs
?stm15				lda	rcolv,Y			;Set at right wall (slightly in)
					clc
					adc	#01
					sta	objxl,Y
					txa
					sta	rtcol,Y
				endif
			endif
		endif
bad12		rts	

;*********************************************************
;* Left and Right wall check values. The following table *
;* is used to compensate for the different size of object*
;*********************************************************
lcolv		.byte lftwal+4		;Man
		.byte lftwal		;Reactor Place keeper
		.byte $88,$88,$88,$88
		.byte $88,$88,$88,$88
		.byte $88,$88,$88,$88
		.byte $88,$88,$88,$88
		.byte $88,$88,$88,$88
		.byte $88,$88,$88,$88
		.byte $90,$90,$90,$90
		.byte $98,$98,$98,$98
		.byte $98,$98,$98,$98,$98,$98
		.byte $90,$90,$90,$90,$90,$90,$90,$90,$90,$90

rcolv		.byte $48			;Man
		.byte $48			;Reactor Place keeper
		.byte $58,$58,$58,$58
		.byte $58,$58,$58,$58
		.byte $58,$58,$58,$58
		.byte $58,$58,$58,$58
		.byte $58,$58,$58,$58
		.byte $58,$58,$58,$58
		.byte $58,$58,$58,$58
		.byte $58,$58,$58,$58
		.byte $58,$58,$58,$58,$58,$58
		.byte $68,$68,$68,$68,$68,$68,$68,$68,$68,$68
		
;*************************************************************
;* Using above stamp, check to see if it is possible to pass *
;* above a stamp 6. This is possible if there is a space but *
;* certain stamps leave no space here as say another 6.      *
;* If below table has an 80, then there is no space to pass  *
;* above.                                                    *
;*************************************************************
abvchk	.byte $00,$00,$80,$00,$00,$80,$80,$00

;****************************************
;* Stamp 7 - Blank                      *
;* No H action, must check stamps on    *
;* either side to see if it's time      *
;* to fall                              *
;****************************************	
stamp7	ldx	#00		;Will guess no ground
		lda	temp3		;LSB X position
		ifmi			;On right edge of blank
			jsr	rtchk		;Check right side
		else
			jsr	ltchk		;Check left side
		endif
		txa	
		sta	ground,Y
		rts
		
;****************************************************
	.sbttl "Right Edge Check"
;****************************************************
;* Check stamps to the right for possible motion    *
;* restriction when the man is 'underground' level  *
;*                                                  *
;* Inputs:	undg = 80 if under level                *
;* 		temp3= X position (word)                *	
;*          temp4= Y position (word)                *
;*          x = 0                                   *
;*                                                  *
;* Output:  rtcol = 80 if not right motion allowed  *
;*        	X = (tgrnd) or 0 depending on           *
;*              conditons of collisions             *
;*                                                  *
;* Uses: 	temp5(word), 2 bytes of stack           *
;****************************************************
rtchk		inc	temp3+1		;Look 1 stamp right
		jsr	sttype
		dec	temp3+1		;Restore original value
		cmp	#04			;Look for 0,1,2,3 as bad (0=dont'care)
		ifcc
			lda	temp3			;Where are we::
			cmp	#lftwal+$3e
			ifcs
				bit	undg,abs		;Underground?
				ifmi
					lda	#$80
					jsr	srtcol
				endif
				ldx	tgrnd			;This may be used if in 'st7'
			endif
		endif
		rts
		
;****************************************************
	.sbttl "Left Edge Check"
;****************************************************
;* Check stamp to the left for possible motion      *
;* restriction when the man is 'underground' level  *
;*                                                  *
;* Inputs:	undg = 80 if under level                *
;* 		temp3= X position (word)                *	
;*          temp4= Y position (word)                *
;*          x = 0                                   *
;*                                                  *
;* Output:  rtcol = 80 if no left motion allowed    *
;*        	X = (tgrnd) or 0 depending on           *
;*              conditons of collisions             *
;*                                                  *
;* Uses: 	temp5(word), 2 bytes of stack           *
;****************************************************	
ltchk		dec	temp3+1
		jsr	sttype		;What stamp is to the left
		inc	temp3+1		;Restore
		cmp	#01
		ifeq	
bad3			lda	temp3
			cmp	#rtwal-$30		;Was a floor to the left??
			ifcc				;yep
				bit	undg,abs		;under floor level?
				ifmi				;yep
					lda	#$80
					jsr	sltcol		;Store to proper ltcol
				endif
				ldx	tgrnd
			endif
		else				;Not stamp 1, check other bad ones
			cmp	#04			;>=4??
			ifcs
				cmp	#06
				ifcc		
					jmp	bad3			;Was 4 or 5
				endif
			endif
		endif
		rts
		
;************* Store Proper ltcol ******************	
sltcol	ldy	zindex
		sta	ltcol,Y
		rts	
;************* Store Proper rtcol ******************
srtcol	ldy	zindex
		sta	rtcol,Y
		rts	
;************* Bit Proper ltcol ******************
bitlt		ldy	zindex
		lda	ltcol,Y
		rts	
;************* Bit Proper rtcol ******************
bitrt		ldy	zindex
		lda	rtcol,Y
		rts	
;*************   Set Left Edge  ******************
setlft	lda	lcolv,Y		;Get Left edge for this guy
		clc	
		adc	#$1			;Move in 1
		sta	oldxl,Y		;Fake to show right of left edge
		rts	
;*************  Set Right Edge  ******************
setrt		lda	rcolv,Y		;Get Right edge for this guy
		sec	
		sbc	#$1			;Move in 1
		sta	oldxl,Y		;Fake to show left of right edge
		rts	
		
;*******************************************
	.sbttl  "Head Case Table"  
;*******************************************
;* Head collision case routine. Uses info  *
;* in temp3 and temp4 (X & Y Man Position) *
;* and headcol to determine velocity action*
;*******************************************
coljs2	.word hd0-1
		.word hd1-1	
		.word hd2-1
		.word hd3-1
		.word hd4-1
		.word hd5-1
		.word hd6-1
		.word hd7-1
colje2	;End of table marker

docse2	lda	mzgame,abs
		and	#02
		ifeq				;Not in hold
			lda	abvstmp		;Stamp above head
			asl	a			;Words
			tax	
			cpx	#colje2-coljs2
			ifcs
				ldx	#02
			endif
			lda	coljs2+1,X
			pha
			lda	coljs2,X
			pha
		endif
		rts
		
;*************************************************
	.sbttl "Special Stamp Collisions for Head"
;*************************************************
; Stamp 0 	
hd0		rts			;No Action!!!

; Stamp 1
hd1		bit	headcol
		ifmi				;Possible Head Collision?
			lda	#00			;Collision clears jump bit
			sta	jumpst
			bit	tumble		;First time check on head?
			ifpl				;yep, do this once
				lda	mazvyl		;Slow down 'up' velocity
				ifpl				;Only slow 'up' velocities
					cmp	#$80
					ror	A
					sta	mazvyl		;Will cut it in half
				endif
			endif
			lda	picseq
			and	#04			;Jump??
			ifne				;yes
hd12				lda	#$10
				sta	piccur		;Hit head, crouch!!
				lda	#$80
				sta	tumble		;He's going to tumble
				lda	#snd_i2c
				jsr	dosound		;Klunk Noise
				lda	#fallseq		;And make him fall
				sta	picseq		;Down!!!!!!!
			endif
		endif
		rts	

;**********************************************************
;* Stamp 3 - H Line, Right Edge Up.                       *
;* Hit head on the left, allow to pass on the right       *
;**********************************************************
hd2		;* Stamp 2 - H Line, Right Edge Down          *
		;* Same as 3 except for no left skewer check  *
		;********************************************** 
		jsr	hd62		;Must check turn down part first
hd3		lda	temp3		;X LSB Value
		cmp	#lftwal-$10	
		ifcc
			bit	mazhed
			ifmi
				jsr	hd1		;Hit Head
			endif
			bit	abvg,abs
			ifmi
				lda	abvstmp
				cmp	#03
				ifeq				;Stamp 3 differs
					lda	#$80			;No left motion allowed
					jsr	sltcol
				endif
			endif
		else
			bit	abvg,abs		;Above ground??
			ifmi
				jsr	rthdck			;Check stamp to right
			endif
		endif
		rts

;**********************************************************
;* Stamp 4 - H Line, Left Edge Up.                        *
;* Hit head on the right, allow to pass on the left       *
;**********************************************************
hd5		;* Stamp 5 - H Line, Left Edge Down           *
		;* Okay to pass above ground here.            *
		;********************************************** 			
		jsr	hd62			;Must check part that turns down first
hd4		lda	temp3			;X LSB value
		cmp	#rtwal+$10
		ifcs
			bit	mazhed
			ifmi
				jsr	hd1		;Hit head
			endif
			bit	abvg,abs			;Above Ceiling??
			ifmi
				lda	abvstmp
				cmp	#04			;No right motion on 4
				ifeq
					lda	#$80
					jsr	srtcol
				endif
			endif
		else				;On left edge of stamp
			bit	abvg,abs		;Above ceiling??
			ifmi
				jsr	lthdck		;Check next stamp over
			endif
		endif
		rts
		
;**********************************************************
;* Stamp 6 - Vertical Wall                                *
;* Must check to see if we hit it from the bottom as well *
;* as no horizontal motion through it.                    *
;**********************************************************	
hd6		jsr	hd7			;Check sides as in 7
hd62		bit	ground		;This is entry for any stamp that has an edge pointing down
		ifpl	
			lda	temp3
			ifmi
				cmp	#lftwal		;Hit Wall?
				ifcc
					lda	#$80
					jsr	sltcol
				endif
			else
				cmp	#$rtwal
				ifcs
					lda	#$80
					jsr	srtcol
				endif
			endif
			lda	temp4			;Y LSB
			cmp	#gndv+$10		;Ground value
			ifcs
				lda	temp3			;Hit head check
				cmp	#lftwal-$18
				ifcc
					cmp	#rtwal+$10
					ifcs
						lda	#00
						sta	jumpst	;Clear Jump
						jmp	hd12		;Hit head
					endif
				endif
			endif
		endif
		rts
		
;**********************************************************
;* Stamp 7 - Blank                                        *
;* Must check the stamp to the closest side to see if a   *
;* possible collision might have occured. This is similar *
;* to st7 for ground.                                     *
;**********************************************************	
hd7		ldx	#00		;Will guess no head collision
		lda	temp3		;LSB X Position
		ifmi			;left edge
			jsr	bitrt		;Already have a collision
			ifpl			;no
				jsr	rthdck	;check right side
				jsr	bitrt
				ifmi
					jsr	hd1		;yes
					lda	abvg,abs	;Set back to 0 if needed (was a flag here)
					jsr	srtcol
				endif
			endif
		else
			jsr	bitlt		;Already have a collision?
			ifpl			;if yes, skip this
				jsr	lthdck	;Check left side
				jsr	bitlt
				ifmi			;sure did
					jsr	hd1
					lda	abvg,abs
					jsr	sltcol
				endif
			endif
		endif
		rts
		
;********************************************
	.sbttl "Right Edge Check"
;********************************************
;* Checks one line up and one stamp right   *
;* for possible head and or body collision  *
;*                                          *
;* Inputs: temp3 = X position (word)        *
;*         temp4 = Y position (word)        *
;*                                          *
;* Output: trcol = 80 if no right motion    *
;*                 allowed                  *
;*                                          *
;* Uses:   temp5, 2 bytes stack             *
;********************************************	
rthdck	inc	temp4+1		;One line up
		inc	temp3+1		;One stamp right
		jsr	sttype		;A = What stamp is to the right??
		dec	temp3+1		;Restore
		cmp	#04
		ifcc				;0,1,2,3 collide ( 0 is don't care)
			lda	temp3
			cmp	#lftwal+$3e		;Might be touching next wall
			ifcs				;yep
				lda	#$80
				jsr	srtcol		;right collision
			endif
		endif
		rts
		
;********************************************
	.sbttl "Left Edge Check"
;********************************************
;* Checks one line up and one stamp left    *
;* for possible head and or body collision  *
;*                                          *
;* Inputs: temp3 = X position (word)        *
;*         temp4 = Y position (word)        *
;*                                          *
;* Output: trcol = 80 if no left motion     *
;*                 allowed                  *
;*                                          *
;* Uses:   temp5, 2 bytes stack             *
;********************************************	
lthdck	inc	temp4+1		;One line up
		dec	temp3+1		;One stamp left
		jsr	sttype		;A = stamp to the left
		inc	temp3+1		;restore
		cmp	#01
		ifeq
bad2			lda	temp3
			cmp	#rtwal-$30		;Was floor to left
			ifcc				;yep
				lda	#$80
				jsr	sltcol		;Don't allow to pass left
			endif
		else				;Not 1, check for 4 or 5
			cmp	#04
			ifcs				;>= 4
				cmp	#06			;<6?
				ifcc				;yes
					jmp	bad2			;Was 4 or 5
				endif
			endif
		endif
		rts
		
;*************************************************
	.sbttl "Top Line collisions other than Man"
;*************************************************
;* This routine is for objects moving in    *
;* the maze other than the man. All ground  *
;* and bottom wall collisions for objects   *
;* are handled as if the man, but above     *
;* collisions are not the same.             *
;*                                          *
;* Input:  Y = object index pointer         *
;*         Y,headocl = possible head col    *
;*                                          *
;* Output: Will set proper collision bits   *
;********************************************	
; Top Line case table		
colts2	.word	td0-1
		.word td1-1
		.word td2-1
		.word td3-1
		.word td4-1
		.word td5-1
		.word td6-1
		.word td7-1
colte2	;End of table marker
topcse	lda	abvstmp		;Stamp we might hit
		asl	a
		tax	
		cpx	#colte2-colts2
		ifcs
			ldx	#02
		endif
		lda	colts2+1,X
		pha	
		lda	colts2,X
		pha	
		rts				;Do case
	
td0
td1		rts				;Stamp 0 & 1 always collide so leave headcol alone
	
td2
td3		lda	temp3			;Still contains position
		cmp	#lftwal-$10	;See if we can pass
		ifcs
			lda	#00
			sta	headcol,Y
			bit	abvg,abs			;Possible skewer
			ifmi
				jsr	rthdck
			endif
		endif
		rts
			
td4
td5		lda	temp3
		cmp	#rtwal+$10
		ifcc
			lda	#00
			sta	headcol,Y		;Else pass on this side
			bit	abvg,abs
			ifmi
				jsr	lthdck
			endif
		endif
		rts
			
td6		ldx	#00		;No collision??
		lda	temp4		;Y LSB, above center??
		ifmi			;yes
			lda	temp3		;which side
			ifpl			;Left or center
				cmp	#$58		;At right wall
				ifcs			;yes
					ldx	#$80
					stx	rtcol,Y	;Stop right
				endif
			else			;Right of center
				cmp	#$98		;At left wall
				ifcc			;yes
					ldx	#$80
					stx	ltcol,Y	;Stop left
				endif
			endif
		endif
		txa	
		sta	headcol,Y
		rts
			
td7		ldx	#00		;Guess no head collision
		lda	temp3		;LSB X position
		ifmi
			jsr	bitrt		;skip if already in collision
			ifpl
				jsr	rthdck	;check next stamp right
				jsr	bitrt
				ifmi
					lda	headcol,Y	;Will need to hit if we did
					tax
					lda	abvg,abs	;Set this back if set
					jsr	srtcol
				endif
			endif
		else
			jsr	bitlt		;skip if already in collision
			ifpl
				jsr	lthdck
				jsr	bitlt
				ifmi
					lda	headcol,Y
					tax
					lda	abvg,abs
					jsr	sltcol
				endif
			endif
		endif
		txa	
		sta	headcol,Y		;Set collisions
		rts
		
;********************************************************
	.sbttl "Picture Tables"
;********************************************************
;* The entry tells the number of frames to pass
;* before stepping to the next picture in that seq.		
			
deltbl	.byte 03,04,03,02		;stop,walk,jog,run
		.byte 03,03,03,03		;same as above for jump
		.byte 05,05,05,05		;same as above for fall
		.byte 04,04,04,04		;land

;Pointer to sequences
pictbl	.word	stop,walk,jog,run
		.word jump1,jump1,jump2,jump2
		.word fall,fall,fall2,fall2
		.word land

;Sequence tables - If 80 is added to a number that means
;                  we may wish to add a foot tap sound here.

xxx4 	= 0		;Number each sequence

stop		.byte $01,$02,$19,$02,$05,$17,$17,$06,$19,$19
		.byte $17,$17,$0D,$0E,$0F,$18,$18,$18,$18,$02
		.byte $13,$18,$18,$16,$18,($1A+$80),$18,$1B,$1C
		.byte $00,$00,$00,$00,$00,$00		;States Never Happen
		.byte $24,$25,$0C				;Smack Wall
		
stopseq	= xxx4
xxx4		.set xxx4+1

walk		.byte $01,$02,$09,$05,$05,$06,$0B,$01,$02,($0A+$80)
		.byte $06,($08+$80),$09,$0E,$15,$0E,$15,$12,$15,$02
		.byte $13,$14,$14,$16,$14,$1A,$18,$18,$18
		.byte $00,$00,$00,$00,$00,$00		;States Never Happen
		.byte $24,$25,$0C				;Smack Wall
		
walkseq	= xxx4
xxx4		.set xxx4+1

jog		.byte ($01+$80),$02,$03,($05+$80),($05+$80),$06,$07,($01+$80),$02,$0A
		.byte $06,$08,$09,$0E,$13,$13,$13,$10,$13,$02,$13,$13,$13,$02,$13,$02
		.byte $13,$18,$18
		.byte $00,$00,$00,$00,$00,$00		;States Never Happen
		.byte $24,$25,$0C				;Smack Wall
		
jogseq	= xxx4
xxx4		.set xxx4+1

run		.byte ($01+$80),$02,$03,$04,($05+$80),$06,$07,$00,$02,$0A
		.byte $06,$07,$0D,$0E,$13,$13,$13,$10,$13,$02
		.byte $13,$13,$13,$02,$13,$02,$13,$18,$18
		.byte $00,$00,$00,$00,$00,$00		;States Never Happen
		.byte $24,$25,$0C				;Smack Wall
		
runseq	= xxx4
xxx4		.set xxx4+1
;Jump Table 1 for jump from stop or walk
jump1		.byte $01,$02,$19,$02,$05,$06,$0B,$0B,$09,$02
		.byte $19,$19,$11,$0E,$0F,$0F,$0F,$0E,$0F,$02
		.byte $13,$14,$18,$16,$0D,$1A,$18,$18,$18
		.byte $00,$00,$00,$00,$00,$00		;States Never Happen
		.byte $24,$25,$0C				;Smack Wall
		
stjmseq	= xxx4	;Jump from stop sequence
xxx4		.set xxx4+1
wajmseq	= xxx4	;Jump from walk sequence
xxx4		.set xxx4+1

;Jump Table 2 for jump from jog or run
jump2		.byte $00,$02,$03,$04,$04,$06,$07,$00,$02,$0A
		.byte $06,$07,$0D,$0E,$15,$15,$10,$10,$15,$02
		.byte $13,$14,$14,$16,$14,$1A,$18,$18,$18
		.byte $00,$00,$00,$00,$00,$00		;States Never Happen
		.byte $24,$25,$0C				;Smack Wall
		
jojmseq	= xxx4	;Jump from Job sequence
xxx4		.set xxx4+1
rujmseq	= xxx4	;Jump from Run sequence
xxx4		.set xxx4+1

fall		.byte $01,$02,$19,$02,$05,$17,$17,$06,$19,$19
		.byte $17,$17,$0D,$0E,$0F,$18,$10,$18,$18,$02
		.byte $13,$18,$18,$16,$18,$1A,$18,$18,$18
		.byte $00,$00,$00,$00,$00,$00		;States Never Happen
		.byte $24,$25,$0C				;Smack Wall
		
fallseq	= xxx4	;Fall from Stop
xxx4		.set xxx4+1
waflseq	= xxx4	;Fall from Walk
xxx4		.set xxx4+1

fall2		.byte $00,$02,$03,$04,$04,$06,$07,$00,$02,$0A
		.byte $06,$07,$0D,$0E,$15,$15,$10,$10,$15,$02
		.byte $13,$14,$14,$16,$14,$1A,$18,$18,$18
		.byte $00,$00,$00,$00,$00,$00		;States Never Happen
		.byte $24,$25,$0C				;Smack Wall
		
joflseq	= xxx4	;Fall from Jog
xxx4		.set xxx4+1	
ruflseq	= xxx4	;Fall from Run
xxx4 		.set xxx4+1

land		.byte $01,$02,$19,$02,$05,$17,$17,$06,$19,$19
		.byte $17,$17,$0C,$0C,$0F,$10,$11,$12,$18,$02
		.byte $13,$18,$18,$16,$18,$1A,$18,$18,$18
		.byte $00,$00,$00,$00,$00,$00		;States Never Happen
		.byte $24,$25,$0C				;Smack Wall
		
landseq	= xxx4	;Land from Stop or Walk
xxx4		.set xxx4+1
		
;Teetering Pictures

teetpc	.byte mpic7,  mpic7,   mpic7,   mpic7+4
		.byte mpic7,  mpic7+4, mpic7+4, mpic7+4
		.byte mpic7,  mpic7+4, mpic7,   mpic7
		.byte mpic7+4,mpic7+4, mpic7+4, mpic7
		
;*********************************************************
	.title "TWThings: Things in the Maze"
	.sbttl "Cannon Routines!"
;*********************************************************

cannon	lda	cannp1-zcann,X
		sta	temp5
		lda	cannp2-zcann,X
		sta	temp5+1		;Set up pointer to action table
		lda	#00
		sta	perm5			;Set to one if cannon fires this frame
		lda	#01
		sta	rompg
		ldy	#03
		lda	(temp5,Y)
		sta	perm1			;Store Y MSB of laser cannon
		lda	objst+zstuf+1
		ifmi				;Boots being put on?
			lda	cannss-zcann,X
			jmp	?cm20
		endif
		lda	cannss-zcann,X
		ifne
			cmp	#$11			;Getting ready to fire, don't fetch
			ifcc
				jmp	?cm10
			endif
		endif
		lda	cannfr-zcann,X	;Frame wait timer
		ifeq				;0 means back to start of table
			ldy	cannin-zcann,X
			lda	(temp5,Y)
			ifeq
				ldy	#04			;Since first four bytes are not actions
				lda	(temp5,Y)
			endif
			ifpl				;Turn a gun to a new orientation
				jsr	cannturn
			else
				and	#$40
				ifeq				;Move gun
					lda	(temp5,Y)
					asl	a
					asl	a
					sta	cannfr-zcann,X		;Set wait timer
					ifeq
						sta	velxh,X			;If no frames then stop velocity
						sta	velyh,X
					else
						iny
						lda	(temp5,Y)
						sta	velxh,X
						iny
						lda	(temp5,Y)
						sta	velyh,X			;Set cannon velocities
					endif
				else
					lda	(temp5,Y)
					asl	a
					asl	a
					sta	cannfr-zcann,X		;Or just wait
				endif
				iny
			endif
			tya
			sta	cannin-zcann,X		;Set forward the index an appropriate amount
		else
			dec	cannfr-zcann,X		;Else just decrement wait timer
		endif
?cm10		lda	cannss-zcann,X		;If spinning or firing continue that as well
		ifne
			pha
			ldy	canndf-zcann,X
			clc
			adc	?cm110,Y			;Shot sequence occurs faster
			and	?cm115,Y
			sta	cannss-zcann,X
			pla
			cmp	#$10
			ifeq	
				jsr	cannshot			;Shoot a bolt(routine must return an 8 in A)
			else
				cmp	#$30				;Done spinning
				ifeq
					lda	#00
					sta	cannss-zcann,X
				endif
			endif
		endif
?cm20		lsr	a			;If not shots fired,then this is zero
		tay
		lda	cannseq,Y
		and	#$C0			;Barrel Data
		sta	tempa
		lda	canngr-zcann,X
		and	#$0F			;Preserve Angle
		ora	tempa
		sta	tempa			;Angle and Barrel
		lda	frame
		and	#06
		asl	a
		asl	a
		asl	a
		ora	tempa			;Angle & Barrel and Tubing
		sta	canngr-zcann,X
		lda	cannseq,Y
		and	#$0F
		ora	#$F0			;Tubing Color
		sta	perm1+1
		lda	#02
		sta	rompg
		jsr	movtn2		;Move it, even if clock hit
		lda	#00
		ldx	#$60
		jsr	vgadd2
		ldx	temp9
		jsr	locate
		tya	
		ifpl
			jmp	?cm100		;If offscreen, don't draw it!
		endif
		ldx	temp9
		lda	objyh,X
		cmp	perm1			;If NE, Roof is far away
						;If EQ, Roof is Close
		lda	objyl,X
		ror	A
		clc	
		adc	#08
		sta	temp5			;Distance to roof
		lda	canngr-zcann,X
		sta	temp5+1		;Store this since we lose X now
		lda	#$30
		ldx	#$71
		jsr	vgadd2
		lda	#$A7
		ldx	#mzls7
		jsr	vgadd2		;White
		lda	#00
		sec	
		sbc	temp5
		ldx	#00
		jsr	vgadd2		;Up To Roof
		lda	#00
		ldx	#$20			;Bright White Line
		jsr	vgadd2
		lda	#$F1
		ldx	#mzls7
		jsr	vgadd2		;Dark Blue
		lda	cann
		ldx	cann+1
		jsr	vgadd2		;Draw Cannon Mount
		lda	temp5
		ldx	#$1F			
		jsr	vgadd2		;Back down from roof
		lda	#00
		tax	
		jsr	vgadd2		;Invisible Line
		ldy	#mzls7
		lda	temp5+1
		and	#$0E
		cmp	#08
		ifcs
			ldy	#mzls7+4		;Need Xflip if angle is 8-0
			eor	#$FF
			adc	#$0C
		endif
 		sty	tempa			;Xflip
		sta	tempa+1		;Rotation Info, 0 2 4 6
		lda	#$C6
		ldx	tempa
		jsr	vgadd2		;Yellow
		lda	#$58
		ldx	#$71
		jsr	vgadd2		;Scale down cannon picture
		ldy	tempa+1
		lda	cann+2,Y
		ldx	cann+3,Y
		jsr	vgadd2		;Cannon Body
		lda	#$AB
		ldx	tempa
		jsr	vgadd2		;Red
		lda	temp5+1
		and	#$C0
		lsr	A
		lsr	A
		lsr	A
		adc	tempa+1
		tay	
		lda	cann+$0a,Y
		ldx	cann+$0b,Y
		jsr	vgadd2		;Barrel
		lda	perm1+1
		ldx	tempa
		jsr	vgadd2		;Cyan (usually)
		lda	temp5+1
		and	#$30
		lsr	A
		adc	tempa+1
		tay	
		lda	cann+$22,Y
		ldx	cann+$23,Y
		jsr	vgadd2		;Tubing
		lda	perm5
		ifne				;It shot this frame
			lda	#snd_i7b
			jsr	dosound
		endif
?cm100	lda	#02
		sta	rompg
		rts
		
?cm110	.byte $01,$02,$04
?cm115	.byte $FF,$FE,$FC

;*********************************************
	.sbttl "Turn Cannon to new Orientation"
;*********************************************
cannturn	sty	tempa			;Save Index
		sta	tempa+1		;Save Data
		pha	
		and	#06			;Get Difficulty
		lsr	A
		tay	
		sta	canndf-zcann,X	;Store it away
		lda	?ct90,Y
		sta	perm4
		lda	?ct91,Y
		sta	perm2
		lda	?ct92,Y
		sta	perm3
		pla	
		and	#$38
		lsr	A
		lsr	A
		lsr	A
		tay				;New Orientation Goal.. 0 to 6
		lda	cannss-zcann,X
		bne	?ct70			;Don't do this if spinning
		lda	canngr-zcann,X
		and	#$0E
		cmp	?ct100,Y		;Current Cannon angle versus final angle
		ifne
			php				;Wrong direction
			lda	objyl,X
			and	perm4
			cmp	#$A0
			ifne				;Wrong Height to rotate at
				ifcs
					sbc	perm2			;Too high so lower it
				else
					adc	perm2			;Too low so raise it
				endif
				sta	objyl,X
				plp
			else
				plp			;Right height, so rotate it
				ifcs			
					lda	#$FE		;Too large a number so lower it 
					clc
				else
					lda	#02		;Too small a number so raise it
				endif
				tay
				lda	frame		;Turn more slowly
				and	perm3
				ifne
					ldy	#00
				endif
				tya
				adc	canngr-zcann,X
				sta	canngr-zcann,X
			endif
		else
			lda	objyl,X		;Correct direction, wroing height
			and	perm4
			cmp	?ct110,Y
			beq	?ct80			;Made it!
			ifcs
				sbc	perm2
			else
				adc	perm2
			endif
			sta	objyl,X
			cmp	?ct110,Y
			beq	?ct80
		endif
?ct70		ldy	tempa			;Restore Pointer
?ct75		rts

	
?ct80		lda	tempa+1		;Made it to destination
		and	#01
		ifne
			lda	cannss-zcann,X		;Cannon spinning?
			ifne
				lda	#$10				;Fire Immediately
			else
				lda	#01				;Warm up and fire
			endif
			sta	cannss-zcann,X
		endif
		inc	tempa 		;Move pointer forward one, either to shot
						;velocity or new instruction bra
		bne	?ct70
		
?ct90		.byte $fc,$f8,$f0
?ct91		.byte $04,$08,$10
?ct92		.byte $03,$01,$00
?ct100	.byte $00,$00,$00,$06,$0C,$0C,$0C		;Angles
?ct110	.byte $C0,$80,$40,$80,$C0,$80,$40		;Heights

cannseq	.byte $03,$04,$03,$06,$04,$06,$0B,$06	;Warm up to fire
		.byte $8B,$8B,$46,$4B,$0B,$06,$04,$06	;Fire and retract Muzzle
		.byte $04,$06,$04,$06,$03,$04,$03,$06	;Re-extend Muzzle


cannshot	ldy	#zlsht+nmlsht-1		;Find a place to put shot
		begin	
			lda	objst,Y
			ora	limbo,Y
			beq	?cs10
			dey	
			cpy	#zlsht
		ccend
		inc	cannin-zcann,X		;Nowhere to put it
		lda	#$10
		rts	
		
?cs10		sty	tempa
		inc	perm5
		lda	canngr-zcann,X		;Shot this frame
		and	#$0E
		lsr	A
		cmp	#03
		ifeq
			lda	#08
		endif
		lsr	A
		lsr	A
		sta	tempa+1
		tay	
		lda	objxl,X
		clc	
		adc	?cs100,Y
		ldy	tempa
		sta	objxl,Y
		lda	objxh,X
		ldy	tempa+1
		adc	?cs101,Y
		ldy	tempa
		sta	objxh,Y
		lda	objyl,X
		clc	
		ldy	tempa+1
		adc	?cs102,Y
		ldy	tempa
		sta	objyl,Y
		lda	objyh,X
		ldy	tempa+1
		adc	?cs103,Y
		ldy	tempa
		sta	objyh,Y
		ldy	cannin-zcann,X		;Get Velocity
		inc	cannin-zcann,X
		lda	(temp5,Y)
		sta	temp9+1
		lda	#00
		ldy	tempa
		sta	velxh,Y
		sta	velyh,Y
		lda	#$10
		sta	objst,Y			;Temporary Object
		lda	tempa+1
		ifeq
			lda	temp9+1
			sta	velxh,Y		;Apply velocity in correct direction
		else
			cmp	#01
			ifeq
				lda	#00
				sbc	temp9+1
				sta	velxh,Y
			else
				lda	#00
				sbc	temp9+1
				sta	velyh,Y
			endif
		endif
		lda	#$10
		rts
		
?cs100	.byte $68,$98,$00			;Offsets of shots from gun center
?cs101	.byte $00,$FF,$00
?cs102	.byte $00,$00,$80
?cs103 	.byte $00,$00,$FF

;*************************************************************
	.sbttl "Get Scaled Speed"
;*************************************************************
;* The routine calculates the speed an object will move on   *
;* the screen based on it's Y position.(starspeed,etc)       *
;*                                                           *
;* Inputs:	A = Y position MSB                               *
;*        	Y = Y Position LSB					 *
;*   (spcspd) = Speed Factor, larger numbers are slower      *
;*      carry = set for up screen                            *
;*                                                           *
;* Output:	A & temp1  = speed amount LSB                    *
;*          A & temp+1 = speed amoutn MSB			       *
;*                                                           *
;* Uses:	A, Y, temp1, 2 bytes stack                       *
;*                                                           *
;* Note: Entry at gss2 assumes:                              *
;*		1) LSB already stored in temp1                   *
;* 		2) Input Y = speed factor                        *
;*                                                           *
;* Note: Entry at gss1 is for Star Motion. The Amount of     *
;*       offset is added so stars move at different speeds   *
;************************************************************* 
gss1		sty	temp1
		php	
		pha	
		lda	strflg,X		;Save A and Carry
		and	#03
		eor	#$FF			;Negate this
		sec	
		adc	spcspd
		tay	
		pla	
		jmp	gss3
gss		sty	temp1			;Save for later
		ldy	spcspd
gss2		php				;Save entry carry
gss3		jsr	mror			;Get scaled speed
		sta	temp1+1		;Save MSB
		tay				;Also save in Y
		lda	temp1			;MSB=0?? (in case LSB is!!)
		ifeq				;Don't get stuck at 0
			lda	#01
			sta	temp1
		endif
		plp				;Check for negate
		ifcs				;Negate
			jsr	dblneg
			tay			;Save MSB in Y
			lda	temp1
		endif
		rts
		
;*************************************************************
	.sbttl "Get Intensity .vs Distance"
;*************************************************************
;* Gets intensity vs distance 'up' the screen so far away    *
;* objects don't look as bright as objects which are near.   *
;*                                                           *
;* Inputs:	A = Distance up Screen, not corrected for star   *
;*        	    origin offset. 					 *
;*                                                           *
;* Output:	A = Intensity byte for Stat Instruction          *
;*                                                           *
;* Uses:	A                                                *
;************************************************************* 			
getint	clc	
		adc	#04		;Add 4 to give range of 4-f
		asl	a	
		cmp	#$10
		ifcs
			lda	#$0F
		endif
		asl	a
		asl	a
		asl	a
		asl	a
		rts			;Intensity in top nibble!
			
;*****************************************
 .sbttl "Stars"
;*****************************************
escape	jmp	strmov	;Will just move them this time

;*****************************************
	.sbttl "Generate Stars"
;*****************************************
strgen	lda	mzgame	
		cmp	#04		;Possible Landing?
		ifeq			;Could be, check direction
			bit	gamest	;Up or Down??
			bvc	escape	;Just draw them
		endif
		cmp	#08		;Transistion to up or down
		beq	escape	;Just draw
		and	#02+$80	;Skip on Zoom or maze
		ora	tact		;Skip if tactical
		ifne
			rts			;Skip on these games
		endif	
		lda	numstr
		cmp	#maxstr
		bcs	escape	;No start up's needed
		ldx	#maxstr
		begin
			dex
			bmi	escape	;None available now
			lda	strflg,X
		eqend			;Found a dead one
		lda	mzgame	
		and	#04		;3rd person view?
		ifne
			jsr	getrand
			sta	strxl,X
			jsr	getrand
			and	#07
			bit	rands+2
			ifmi
				jsr	neg
			endif
			sta	strxh,X
			lda	#00
			sta	stryl,X
			sta	stryh,X	;Start at top
			jmp	?gs9		;*** Always!! ***
		endif
;****** Normal First Person View Stars *******
		jsr	getrand
		sta	stryl,X
		jsr	getrand
		sta	strxl,X		;Got X
		lda	rearview		;Moving Backwards?
		ifpl				;nope
			lda	#00
			sta	stryh,X
			sta	strxh,X
			jsr	getrand
			ifmi
				dec	strxh,X	;Upper byte minus 1
				lda	strxl,X
				jsr	neg
				sta	strxl,X
			endif
			lda	stroyh		;Where is the origin??
			cmp	#05			;Off top of screen??
			bcs	?gs8			;If yes, alway start down
			jsr	getrand
			ifmi
?gs8				dec	stryh,X
				lda	stryl,X
				jsr	neg
				sta	stryl,X
			endif
		else			;Moving Backwards, start at edges
			bit	rands
			ifmi
				lda	stroyh	;Get Origin
				cmp	#05		;If at top, always start at bottom
				bcs   ?gs11		;Always start at bottom
				bit	rands+1
				ifmi
					lda	#06
				else			;Else, start at bottom
?gs11					lda	#-6
					sec
					sbc	stroyh
				endif
				sta	stryh,X	;Place object Y
				jsr	getrand
				and	#$0F
				sec
				sbc	#07		;+/- 7
				sta	strxh,X
			else
				lda	#07		;Guess right edge
				bit	rands
				ifmi
					lda	#-7
				endif
				sta	strxh,X
				jsr	getrand
				and	#07		
				sec
				sbc	#01		;0-6
				bit	rands+2
				ifmi
					jsr	neg
				endif
				sta	stryh,X
			endif
		endif
?gs9		jsr	getrand
		and	#03		;Extra Speed Flags
		ora	#$80		;Flag Active
		sta	strflg,X
		inc	numstr
?gs10		;********* Fall Through ***********

;************************************************
	.sbttl "Move Stars"
;************************************************
;* Will move the stars in a quasi 3 dimensional *
;* pattern, or will move them from a third      *
;* person view if mzgame, bit 4 is set. In this *
;* case, the speed of the stars in Y must be    *
;* passed in ?????                              *
;*                                              *
;* Inputs: mzgame, temp1                        *
;*                                              *
;* Uses:   temp1, temp2, A, X, Y                *
;************************************************
strmov	lda	mzgame
		and	#$5F		;Not during these plays
		ifeq			;Only during space game
			lda	stroyh
			cmp	#06		;Have to move origin
			ifne
				lda	stroyl
				clc
				adc	#$14
				sta	stroyl
				ifcs
					inc	stroyh
				endif
			endif
		endif
		ldx	#maxstr-1		;Total Stars
		begin
			lda	strflg,X
			ifpl
				jmp	nxtstr
			endif
			and	#03
			sta	temp2		;Used in gss1 later
			lda	mzgame
			tay			;Save this
			and	#08		;transition??
			ifne
				jmp	?mst5		;Just draw it
			endif
			tya
			and	#06		;Fixed speed
			ifne
				ldy	#00
				lda	statst	;Move at station speed
				eor	#$FF		;stars are old way, move oppisite
				and	#$7F		;Drop active bit
				cmp	#$40		;Up or down??
				ifcs			;Negative
					dey
					ora	#$80
				endif
				jmp	?mst1		;Move Y only
			endif
			lda	strxh,X
			ldy	strxl,X
			clc			;Down Screen
			jsr	gss1		;Get scaled speed
			bit	rearview	;Backwards??
			ifpl			;Nope
?mst30			clc			;Above returns A=LSB Y=MSB
				adc	strxl,X
				sta	strxl,X
				tya
				adc	strxh,X
				sta	strxh,X
				jsr	chkstr
				beq	nxtstr	;It died
			else			;Else, moving backwards
				sty	temp1+1
				sta	temp1
				jsr	dblneg	;Move inwards
				ldy	temp1+1
				lda	temp1
				bne	?mst30
			endif
			lda	stryh,X	;Y Distance
			ldy	stryl,X
			clc			;Down Screen
			jsr	gss1		;Get Scaled Speed
			bit	rearview	;Backwards??
			ifpl			;nope
?mst1				clc
				adc	stryl,X
				sta	stryl,X
				tya
				adc	stryh,X
				sta	stryh,X
				clc
				adc	stroyh		;Correct for star offset
				jsr	chkstr		;Off Edge
				beq	nxtstr		;Skip if dead
			else
				sty	temp1+1
				sta	temp1
				jsr	dblneg
				ldy	temp1+1
				lda	temp1
				bne	?mst1			;Reverse and move
			endif
?mst5			stx	temp1			;Save X
			jsr	drawstr
			ldx	temp1
nxtstr		dex
		miend
		jmp	shpshts		;Now move ship shots
		;Check star for boudary
chkstr	php				;Save signs	
		bit	rearview		;Moving forward??
		ifpl
			plp				;Recall Signs
			ifmi
				cmp	#-6
				bcc	?mst15
			else
				cmp	#06
				bcs	?mst15
			endif
			lda	#01		;Set NE flag
			rts	
?mst15		dec	numstr
			lda	#00		;Another star novas
			sta	strflg,X	;Return with 0
			rts	
		else
			plp			;Toss away
			lda	strxh,X
			ora	stryh,X	;When both MSB's are 0...
			beq	?mst15	;... Kill it!!
		endif
		rts	
		
drawstr	lda	strxl,X
		sta	xcomp
		lda	strxh,X
		sta	xcomp+1		;Save X vector value
		lda	stryl,X
		clc	
		adc	stroyl
		sta	xcomp+2
		lda	stryh,X
		pha				;Save for intensity
		adc	stroyh		;Center point offset
		sta	xcomp+3		;Transfer position
		jsr	vgcntr		
		lda	#$58			;Use same scale as for ship position
		ldx	#$73
		jsr	vgadd2		;Set Scale
		lda	#00
		sta	vgbrit		
		jsr	vgvtr2
		pla				;Recall Y posit
		ifmi
			eor	#$FF
		endif
		asl	a
		asl	a
		asl	a
		asl	a			;Stars Intensity
		adc	#$47
		ldx	#mapdt7+sparkle	;Stat, Sparkle
		jsr	vgadd2
		lda	#00
		ldx	#$72			;Scale for dot
		jsr	vgadd2
		laljsr(mapdot)
		lxhjsr(mapdot)
		jmp	vgadd2
		
;**********************************************
	.sbttl "Ships Shots Routine"
;**********************************************
;* Shots wil move just opposite of the stars  *
;**********************************************
shpshts	lda	#00
		sta	stbcol		;Guess no collision
		ldx	#nmsshots-1		;Ship shots
		stx	temp9
		begin
			lda	shotst,X		;Shot Active??
			bne	?ssr10		;This one is active
			jmp	?ssr20
?ssr10			ifpl				;Not blowing up!
				lda	shotxh,X
				sta	xcomp+1		;Transfer X position
				lda	shotxl,X
				sta	xcomp
				lda	#shtspd	
				ldy	#-1			;Constant speed up screen
				clc
				adc	shotyl,X		
				sta	shotyl,X		;Save shot Y LSB
				sta	xcomp+2
				tya
				adc	shotyh,X
				sta	shotyh,X
				sta	xcomp+3
				tay				;Save LSB
				iny
				lda	mzgame
				and	#04
				ifeq
					dey
					dey
					dey
				endif
				tya	
				ifmi
					lda	#00
					sta	shotst,X		;Stop This
				else					;Else, draw this one
					jsr	ckbase
					lda	#02			;Size factor
					jsr	drawshot		;Draw the shot (does the position)
				endif
?ssr20		else
				lda	shotxl,X
				sta	xcomp
				lda	shotxh,X
				sta	xcomp+1
				lda	shotyl,X
				sta	xcomp+2
				lda	shotyh,X
				sta	xcomp+3		;Xfer Location
				lda	#02			;Scale Factor
				jsr	drawshot		;Will only position
				ldx	#shtex7
				jsr	vgadd2		;Page select for shot
				ldx	temp9
				lda	shotst,X		;Continue Explosion
				lsr	A
				lsr	A
				lsr	A
				lsr	A
				tay				;Save this
				clc
				adc	shotst,X		;Move along explosion
				ifpl
					lda	#00
				endif
				sta	shotst,X
				tya
				asl	A
				tay
				lda	shtexp-$10,Y	;Shot Explosion
				ldx	shtexp-$10+1,Y	;MSB of JSR
				;-10 as top nibble for explosion goes 80 to off
				jsr	vgadd2
			endif
			dec	temp9
			ldx	temp9
		miend
		
;**********************************************
	.sbttl "Check Fire Switch"
;**********************************************
;* Start a shot from space ship. Will start 1 *
;* shot every time the fire button is pressed *
;* If the button is held, it will fire every  *
;* 1/4 second.(As long as shots are available)*
;**********************************************		
strtshot	lda	shipst
		ifne
			ifpl
				lda	mzgame
				ifeq				;Space only
					lda	gamest		;Attract??
					ifpl				;Yes
						jsr	getrand
					else				;Game Mode
						lda	jbstat		;Button pressed
					endif
					ifmi			;Yes!
						lda	shotdb		;Pressed Last time
						ifpl				;no
							lda	#$80
							sta	shotdb		;Set last flag
							jsr	stone			;Toss a stone
						else
							lda	frame
							and	#03			;Time lag between shots
							ifeq
								jsr	stone			;Toss one out
							endif
						endif
					else			;Button not pressed
						sta	shotdb	;Clear last flag
					endif
				endif
			endif
		endif
		jsr	enemy		;Do Enemy Routines
		jmp	laser		;Do Enemy Ships
		
;***************************************************
	.sbttl "Start a Shot at the Ship"
;***************************************************
stone		ldx	#nmsshots-1
		begin				;Find an empty slot
			lda	shotst,X
			ora	shotst-1,X		;Need 2 slots
			beq	?ss10
			jmp	?ss20			;Long Jump
?ss10			lda	#01			
			sta	temp4
			ldy	shppbs		;Picture base (0-5)
			begin
				lda	shipxl
				sta	shotxl,X
				lda	shipxh
				sta	shotxh,X		;Save X Position
				lda	shipyl
				sta	shotyl,X
				lda	shipyh
				sta	shotyh,X		;Position Shot
				lda	#00
				sta	temp1			;Guess + Number 
				lda	nxtsid		;Alt sides
				eor	#$80
				sta	nxtsid
				ifmi	
					lda	strlfx,Y		;Start Left
				else
					lda	strrtx,Y		;Start right
				endif
				ifmi				;Negative Number??
					dec	temp1			;Prop carry
				endif
				asl	a			;Number *2
				rol	temp1
				clc
				adc	shotxl,X
				sta	shotxl,X
				lda	temp1
				adc	shotxh,X
				sta	shotxh,X		;Correct X
				lda	#-1
				sta	temp1			;The number is -
				lda	#$E4			;Y always from same place ($f2*2)
				clc
				adc	shotyl,X
				sta	shotyl,X
				lda	temp1			;MSB (sign prop)
				adc	shotyh,X
				sta	shotyh,X		;Finish corrected position
				lda	#01
				sta	shotst,X		;Active
				dex				;Start alt shot	
				dec	temp4
				lda	#$80			;No-op out for pair fire
			miend
			lda	#snd_c2
			jmp	dosound
?ss20			dex
			dex
		miend
		rts
		
		
			
strlfx	.byte $E4,$E6,$EA,$EB,$EE,$F0,$F1,$F4,$F6,$F8,$FA,$FC
strrtx	.byte $08,$08,$0C,$0E,$10,$10,$12,$15,$16,$18,$1A,$1C

;**********************************************
	.sbttl "Draw Shot Picture"             
;**********************************************
;* Entry A= LSB Scale Factor                  *
;**********************************************
drawshot	sta	scalef+1		;Size offset
		lda	#00
		sta	scalef
		jsr	posvec		;OK Mark, position and set scale
		ifmi
			rts				;Skip if offscreen
		endif
		ldx	temp9			;Recall X
		lda	shotyh,X		;For intensity
		jsr	getint		;Get shot's intensity
		sta	temp1			;Might want this later
		ldx	temp9			;Skip if this shot is exploding
		cpx	#nmsshots		;Player Ship Shot??
		ifcc				;yes
			ldy	shotst,X
			ifmi
				rts
			endif
		endif
		ldx	#shpsh7		;Add color and page select
		ora	#$0A			;Add color of shot
		jsr	vgadd2
		laljsr(shipsh)
		lxhjsr(shipsh)
		jsr	vgadd2		;Add picture
		lda	#00			;Make sure not -
		rts	
		
;**********************************************
	.sbttl "Check Base"
;**********************************************
;* Check shots against enemy base station for *
;* possible hit. Called from mvenst as each   *
;* shot is moved                              *
;*                                            *
;* Inputs: X = Index of this shot             *
;**********************************************
ckbase	lda	mzgame		;Landing or Takeoff
		and	#04
		ifne
			rts
		endif
		lda	#00
		sta	temp4+1		;LSB 1 in case used
		ldy	maznum		;Which base star
		lda	boszy,Y		;Base 0 Y size
		sta	temp5			;Save Y size
		lda	boszx,Y		;Base 0 X size
		asl	a
		sta	temp4			;X is size over 2
		rol	temp4+1		;Make 1 if overflow
		lda	statxl
		sec	
		sbc	xcomp			;Position left in xcomp
		sta	temp1
		lda	statxh
		sbc	xcomp+1
		sta	temp1+1
		lda	temp1+1		;Reset Sign
		ifmi
			jsr	dblneg		;Abs of X difference
		endif
		lda	temp4
		sec	
		sbc	temp1
		lda	temp4+1
		sbc	temp1+1		;Compare to size
		ifcs				;We hit in X
			lda	statyl
			sec
			sbc	xcomp+2
			sta	temp1
			lda	statyh
			sbc	xcomp+3
			sta	temp1+1
			ifmi
				jsr	dblneg
			endif
			ifeq			;MSB is 0
				sec
				lda	temp1
				sbc	temp5
				ifcc			;We hit it
hitbas				lda	#$80
					sta	shotst,X		;Kill this shot
					sta	stbcol		;Indicate collision at base
					lda	#snd_c3
					jmp	dosound
				endif
			endif
		endif
		rts
		
;****************************************
;* Table of Base Star Sizes             *
;****************************************	
boszx		.byte $51,$78,$FF,$7F
boszy		.byte $80,$90,$90,$A0

;****************************************
;* Rotating Beacon utility. Position at *
;* place of draw and pass color in reg A*
;* Pass beacon number in Y              *
;****************************************
beacon	sta	temp1
		tya				;Y has beacon offset
		clc	
		adc	frame			;Pick Intensity
		and	#$1F
		lsr	a
		lsr	a
		tax	
		pha				;Save this for below
		lda	beaintn,X		;Get Intensity
		ora	temp1			;Put back color
		ldx	#becn7		;Stat, Page Select
		jsr	vgadd2		;Add Color Instruction
		pla				;Recall above
		asl	a			;Words entry
		tay	
		lda	beapic,Y
		ldx	beapic+1,Y		;JSRL to beacon picture
		jmp	vgadd2		;Bye
		
beaintn	.byte $40,$70,$A0,$F0,$A0,$70,$40

;****************************************
	.sbttl "Laser Shot"
;****************************************
;* Base gun (and whoever else wants it) *
;* laser. Will draw a 'slow' vector     *
;* between the 2 points passed in lsdsxx*
;* and lsstxx.                          *
;****************************************
laser		lda	mzgame
		and	#$18			;Get rid of it here
		ifne
			lda	#00
			sta	lasst
?ls1			rts
		endif
		lda	#00
		sta	vgbrit
		lda	lasst			;Active??
		beq	?ls1
		ldx	#07			;Copy laser vals into temps
		begin
			lda	lstsxl,X
			sta	temp4,X
			dex
		miend
		bit	mzgame		;3rd person play??
		ifpl
			jsr	laser3		;Adjust points for 3rd person
		endif
		jsr	sclset		;Position Scale
		jsr	vgcntr		;Center for first point
		lda	#$FA
		ldx	#$60+sparkle	;Stat, Sparkle on
		jsr	vgadd2
		lda	lasst
		sec	
		sbc	laspd			;Laser Speed
		sta	lasst
		ifmi				;Drawing Outward
			jsr	pt1			;Start at Origin
			jsr	vgvtr2		;Position Origin
			jsr	lasz			;At this poing A= 8-F
			sec
			sbc	#08			;Now 2-9
			cmp	#01			;Don't go below 2
			ifcc
				lda	#$6F			;Set beam to move out
				sta	lasst			;Set Status
				lda	#01			;And hold this at 2
			endif
		else				;Moving in
			jsr	pt2			;Point 2 is Origin
			jsr	vgvtr2		
			bit	lascol		;Collision??
			ifmi
				lda	#00
				ldx	#$72
				jsr	vgadd2		;Add a little fire ball
				laljsr(sparkb)
				lxhjsr(sparkb)
				jsr	vgadd2
			endif
			jsr	vgcntr		;Recenter
			jsr	sclset		;Restore Scale
			jsr	pt2
			jsr	vgvtr2		;Reposition for draw back
			jsr	lasz			;Get bin scale figure
			;At this point A = 0-7
			ifeq				;Done
				sta	lasst			;Turn it off
				ldy	#$8A			;Close the gun
				ldx	shipxh		;Look at side of ship
				dex
				dex
				dex
				ifpl
					sty	gunctl+1
				else
					sty	gunctl
				endif
				rts				;And leave
			endif
			eor	#$FF
			and	#07			;Now have 0-7
			cmp	#01			;Don't get too large
			ifcc
				lda	#01
			endif
			cmp	#03			;It's long enough to hit now
			ifcc
				ldx	shipst
				ifne
					ifpl				;Only if not blowing up
						pha				;Save A
						jsr	lascl2		;Always die for now
						pla
					endif
				endif
			endif
		endif
		jsr	addscale
		jsr	dif			;Point back to other point
		lda	#$20
		sta	vgbrit		;Draw this line
		jsr	vgvtr2
		rts

;****************************************
	.sbttl "3rd Person Setup"
;****************************************
;* Corrects lsds** and lsts** for 3rd   *
;* person quad system                   *
;****************************************	
laser3	jsr	pt1			;Just move int xcomp
		jsr	cor3			;Correct this poing
		ldx	#03
		begin
			lda	xcomp,x
			sta	temp6,x
			dex
		miend
		jsr	pt2
		jsr	cor3
		ldx	#03
		begin
			lda	xcomp,x
			sta	temp4,x
			dex
		miend
		rts
		
;****************************************
	.sbttl "Laser/Ship Collision"
;****************************************
;* Check end of laser with ship's       *
;* current position                     *
;****************************************	
lascl2	lda	#$80
		sta	lascol		;Signal Collision
		sta	statst		;Stop any station motion
		jsr	blowship		;Blow into Peices
		lda	#snd_stop
		jsr	dosound
		jsr	dodelay
		lda	#snd_c1
		jmp	dosound
		
;****************************************
	.sbttl "Add Proper Scale"
;****************************************
;* Add scale instruction generated above*
;* and check for a scale greater        *
;* (smaller in size) than 6 as the fast *
;* monitor does not allow scale 7.      *
;*                                      *
;* Input: 	A = scale value (2 thru 7)  *
;*                                      *
;* Output:  Scale inst to VGRAM         *
;****************************************
addscale	clc	
		adc	#01
		cmp	#07
		ifcs			;6 or greater stays 6
			lda	#07
		endif
		ora	#$70		;Make it a scale instruction
		tax	
		cpx	#$70		;At positioning scale??
		ifeq	
			lda	#00		;Stick at proper position
		else
			lda	lasst		;Rest of scale
			ifpl				;Closing
				eor	#$FF
			endif
			asl	a
			asl	a
			asl	a
			and	#$70		;Use bottom for bin scale
		endif
		jmp	vgadd2
		
;*************************************
 	.sbttl "Shift lasst Down 4"
lasz		lda	lasst
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		rts	

;*************************************
 	.sbttl "Copy Point 1 to xcomp"
pt1		ldx	#03			;Point 1 (origin)
		begin
			lda	temp6,X
			sta	xcomp,X
			lda	temp4,X
			sta	temp2,X		;In here for diff
			dex
		miend
		rts
		
;*************************************
 	.sbttl "Copy Point 2 to xcomp"	
pt2		ldx	#03			;Point 2 (target)
		begin
			lda	temp4,X
			sta	xcomp,X
			lda	temp6,X
			sta	temp2,X
			dex
		miend
		rts
		
;*************************************
 	.sbttl "Difference to Point 1"
;************************************
;* Calculates the difference from   *
;* the value in xcomp to point and  *
;* places the results in xcomp      *
;************************************	
dif		sec	
		lda	temp2
		sbc	xcomp
		sta	xcomp
		lda	temp2+1
		sbc	xcomp+1
		sta	xcomp+1
		lda	temp3
		sec	
		sbc	xcomp+2
		sta	xcomp+2
		lda	temp2+3
		sbc	xcomp+3
		sta	xcomp+3
		rts	

;*******************************************************
	.title "TWEnemy"
	.sbttl "Space Wave Entry"
;*******************************************************		
enemy		lda	mzgame
		ifeq
			lda	maznum		;Maze number determines what to do
			asl	a
			tax				;Get proper routine
			lda	rpage,X		;Get page select for this guy
			sta	rompg			;Set proper page
			lda	enrout+1,X
			pha
			lda	enrout,X
			pha
		endif
		rts				;Do Enemy Routine	

enrout	.word enem0-1		;Space Wave 0 - Fishoids
		.word enem1-1		;Space Wave 1 - Fighters
		.word enem3-1		;Space Wave 2 - Star Castle
		.word enem2-1		;Space Wave 3 - Web Spinners

rpage		.byte $02,$00,$00,$00,$00,$00,$00,$00

;********************************************
	.sbttl "Space Wave 1 - Fighters"
;********************************************
enem1		lda	#05
		sta	hitpts		;Fighters = 500
		lda	nenstr		;Need to start any?
		ifne				;yep
			lda	statyh
			cmp	#03
			ifeq				;Hold Mom at 3,80
				lda	statyl
				ifpl				;Should get us here
					lda	#00
					sta	stbflg		;Stop it here
				endif
			endif
emem12		jsr	getempty		;Find an empty slot
			bmi	?sf10			;None Available
			lda	mzgame		;Still in space??
			ifeq				;yes
				bit	lauen			;Okay to launch??
				ifmi				;It's OK!
					lda	frame
					and	#03			;Wait some time
					ifeq				;Okay to start
						lda	sndcue+2,abs	;Then make a sound
						ifeq
							lda	#04
							sta	sndcue+2,abs
							lda	#snd_e1
							jsr	dosound
						endif
						lda	statxl
						sta	sobjxl,X		;Store LSB
						lda	statyl
						sta	sobjyl,X		;LSB Y too
						lda	statxh
						sta	sobjxh,X
						lda	statyh
						sta	sobjyh,X
						lda	#$50			;Turn on & setup bit here only
						sta	sobjst,X
						lda	#00
						sta	sobjs2,X		;0 Second status byte
						inc	nenemy
						dec	nenstr		;Started another one
					endif
				endif
			endif
		else				;No Enemy Left
			lda	statyh
			ifpl				;Get it off the screen
				lda	#$C0
				sta	stbflg
			endif
		endif
?sf10		lda	toolong			;Stalling??
		ifmi
			lda	mzgame			;And still space??
			ifeq
				tay					;Y=0
				lda	frame
				and	#03
				ifeq
					lda	frame
					lsr	a
					lsr	a
					and	#$0F				;Dive another one
					cmp	#nmform+1
					ifcc	
						tay
						lda	sobjs2,Y
						and	#$BF				;Drop any hold bits
						ora	#$10				;Kamikaze time!!
						sta	sobjs2,Y
					endif
					ldy	#$80				;Okay to move down this frame
					lda	nenstr			;Any Left??
					ifne					;Wait for all to Launch
						ldy	#00
					endif
				endif
				sty	stbflg			;Will Slowly move down screen
				jsr	skip				;See if ready to skip
			endif
		endif
		;*********** Fall Through ***************
		
;*************************************************
	.sbttl "Correography"
;*************************************************
;* Move ship according to correography table     *
;*                                               *
;* Data: Each byte (1 for each ship) is broken   *
;* up into two parts. The top nibble is the      *
;* number of frames (x4) to do a move. The bottom*
;* nibble is the direction to move. One of 16    *
;* directions is picked (0=no motion). 1=22.5 deg*
;* (clockwise) up to 15=337.5 deg. Starting from *
;* 90 deg straight up (ie: 1=112.5 deg, 15=67.5) *
;* Note that there is no straight up motion.     *
;*************************************************
coreg		jsr	next			;Start Any??
		ldx	#nmform-1
		stx	temp9
		begin
			lda	sobjs2,X
			sta	temp3+1		;Save 2nd Status
			lda	sobjst,X		;Active??
			sta	temp3			;For later use
			bne	?cor10
?cor6			jmp	?cor20
?cor10		ifmi				;Exploding??
				lda	sobjst,X
				clc	
				adc	#04
				sta	sobjst,X
				ifpl
					jsr	oneless		;One Less
				endif
				jsr	copypos		;Copy Position to xcomp
				jmp	?cor15		;Just draw it
			else				;Else, must be active
				bit	temp3+1		;See if in hold place
				bvs	?cor13		;Yes, don't move it
				bit	temp3			;See if in setup mode
				ifvs
					jsr	setup			;Do setup
?cor13				jmp	?cor28
				endif
				and	#$20			;Force mode??
				ifne				;yes
					lda	toolong		;Stop moving if base is moving
					ifmi
						lda	sobjyh,X		;Already on it's way up??
						cmp	#$0A
						ifcs				;nope
							lda	#00
							sta	sobjst,X		;Just kill it then
							beq	?cor6			;And skip it
						endif
					endif
					lda	sobjyh,X
					ifeq
						jsr	stcorg		;Restart Correography
						ora	#$40
?cor8						sta	sobjst,X		;Put Back into Formation
						sta	temp3			;May be needed later
						jsr	thisent		;Get this correography entry
						sta	sobdir,X
						bne	?cor16		;And move it!
					endif
?cor7					lda	#00			;To Move Up, No X Motion
					pha				;X step is always 0 for up the screen
					ldy	#-1			;Sign extend on Y step
					lda	#-$20			
					bne	?cor11		;Always
				endif
				lda	temp3+1
				and	#04			;To 'start'??
				bne	?cor7			;Yes, get there.
				lda	temp3+1		;Check if 'following'
				and	#$20			;Followbit
				ifne				;It's follow the leader time
					jsr	followme		;Do Follow the leader
					jmp	?cor28
				endif
				lda	temp3+1
				and	#$10			;Kamakazi??
				ifne
					jsr	kama			;Check Update
					bne	?cor16
				endif
				lda	frame
				and	#03			;One every 4 frames
				ifeq
					lda	sobdir,X		;Direction
					sec	
					sbc	#$10
					cmp	#$10
					ifcc				;yes, do next
						jsr	nextent
					endif
					sta	sobdir,X		;new direction
				endif
			endif
?cor16		lda	sobdir,X
			and	#$0F			;Get Direction
			ifeq
?cor28			jsr	copypos		;Just copy position then
			else
				tax
				ldy	#00			;Sign Extend
				lda	cortlx-1,X		;X Step
				pha				;Will do this later
				lda	cortly-1,X		;Y Step
				ifmi
					dey
				endif
?cor11			clc	
				ldx	temp9
				adc	sobjyl,X
				sta	sobjyl,X
				sta	xcomp+2		;Save for Output
				tya
				adc	sobjyh,X
				sta	sobjyh,X
				sta	xcomp+3
				ifmi				;First check for off top
					pla				;Throw away extra push
					lda	temp3+1
					and	#04			;To 'top' of screen??
					ifne
						jsr	stcorg
						sta	temp3
						jsr	thisent
						sta	sobdir,X
					endif
					lda	sobjs2,X
					and	#$FB			;Drop 'startup' bit
					sta	sobjs2,X		;Because it must be at the top
					lda	#center
					sta	sobjxh,X
					lda	#$80
					sta	sobjxl,X
					lda	#00
					sta	sobjyh,X
					sta	sobjyl,X
					beq	?cor28		;Just xfer
				endif
				jsr	pastck		;Check for off 'bottom'
				ldy	#00
				pla
				ifmi
					dey
				endif
				clc
				adc	sobjxl,X
				sta	sobjxl,X
				sta	xcomp
				tya
				adc	sobjxh,X
				sta	sobjxh,X
				sta	xcomp+1		;For Output
				ifmi				;Check for off screen
					lda	#00
					sta	sobjxl,X		;Leave at 0,0
					sta	sobjxh,X
					sta	xcomp
					ldy	#$8A			;If - , it went off the left side
?cor14				sta	xcomp+1		;So it positions right
					lda	sobjst,X		;Forced to top???
					and	#$20			;Skip this if yes
					ifeq				;nope
						tya	
						sta	sobdir,X		;Force back on screen
					endif
				else
					cmp	#rtedge		;Did it hit the right edge
					ifcs				;yep, turn it around!!
						lda	#$FF
						sta	sobjxl,X
						sta	xcomp
						lda	#rtedge-1
						sta	sobjxh,X		;Stick at Left Edge
						ldy	#$85			;Force left then
						bne	?cor14
					endif
				endif
			endif
			jsr	dropshot		;And drop a shot
			jsr	shtchk		;Hit the objects
?cor15		jsr	drawen
?cor20		dec	temp9
			ldx	temp9
		miend
		lda	#00
		ldx	#$60
		jsr	vgadd2		;Restore normal stat
		;Will see if player is stalling and bring out mother if yes
		lda	bonusa		;Out of bonus??
		ora	mzgame		;And we are still in space
		ifeq
			ldx	#nmform-1
			begin
				lda	sobjst,X		;Any left active??
				ifne
					ifpl				;Skip if exploding
						lda	sobjs2,X		;Off bottom once??
						bpl	mvenst
					endif
				endif
				dex
			miend			;If we fall out...
			lda	nenemy		;At least some out?
			ifne
				lda	#$80			;We didn't find any 'good ones'
				sta	toolong		;Set too long timer
				sta	statst		;And turn on the statin
			endif
		endif
		;*************** Fall Through  ******************
		
;*****************************************************
	.sbttl "Move Active Enemy Shots"
;*****************************************************
;* Moves Active Enemy Shots, then calls the drawshot *
;* routine. This routine also checks for shots going *
;* off the bottom.                                   *
;*****************************************************
mvenst	ldx	#zshot+nmshot-1
		stx	temp9
		begin
			lda	objst,X		;Active??
			ifeq
?mes1				jmp	?mes6
			endif
			bmi	?mes1			;Skip if blowing up too
			lda	velyh,X		;See if still moving up?
			ifmi				;Must get it back to normal
?mes10			tay
				lda	velyl,X
				clc
				adc	#$70			;Gravity!!!!
				sta	velyl,X
				tya
				adc	#00
?mes11			sta	velyh,X
			else				;Else, if +, see if fast enough
				ldy	objst,X		;Look at status for...
				cpy	#$10			;Is this a line?
				ifcc
					cmp	stsp			;See if at intended shot speed
					bcc	?mes10		;Keep accelerating then
					lda	stsp			;Max out at this speed then
					bne	?mes11
				else
					cmp	#12d			;Line speed fixed
					bcc	?mes10
					lda	#12d
					bne	?mes11
				endif
			endif
			;Now adjust X velocity to target player
			lda	difcty
			cmp	#04
			ifcs
				lda	#03
			endif
			asl	a
			adc	incdif
			tay				;0,2,4 or 6 plus 0 to 4 gives a 0 to A range
			lda	trkspd,Y		;Tracking speed
			pha
			ldy	#00			;MSB Velocity
			lda	shipxl
			cmp	objxl,X
			lda	shipxh		;See where we are
			sbc	objxh,X		;Which side
			ifcc
				dey				;Prop sign
				pla	
				jsr	neg
			else
				pla
			endif
			jsr	addvel		;Update Velocity
			ldy	#00			;Sign Extend
			lda	velxl,X		;LSB of Velocity
			adc	scstx-zshot,X	;Very LSB position (not displayed)
			sta	scstx-zshot,X
			lda	velxh,X		;MSB of velocity
			ifmi
				dey				;Prop Sign
			endif
			adc	objxl,X
			sta	objxl,X
			sta	xcomp			;Save for Draw and Collision
			tya
			adc	objxh,X
			sta	objxh,X
			sta	xcomp+1
			pha				;Save conditions
			ifeq				;In last one Left??
				lda	velxh,X
				clc
				adc	#03
				sta	velxh,X
			else
				cmp	#rtedge-1		;In last on Right?
				ifeq
					lda	velxh,X
					sec
					sbc	#03
					sta	velxh,X
				endif
			endif
			pla			;Recall Conditions
			ifmi			;If this went -, went of left edge
?mes5				lda	#00
				sta	objst,X	;Just Kill it!
?mes6				jmp	loop2		;And go on to next one
			endif
			cmp	#rtedge	;Off Right Edge of plane?
			bcs	?mes5		;yep
			ldy	#00
			lda	velyl,X
			clc
			adc	scsty-zshot,X
			sta	scsty-zshot,X
			lda	velyh,X
			ifmi	
				dey	
			endif
			adc	objyl,X		;MSB Y velocity
			sta	objyl,X
			sta	xcomp+2
			tya				;Prop Sign
			adc	objyh,X		;Shot Speed in Y (prop carry)
			
;Enter here from TWStar to complete move and do collision and edge checks!
mvst2			sta	objyh,X
			sta	xcomp+3
			;However, check bottom for player ship collision and off bottom
			cmp	#$0B			;Did we go off the bottom
			ifcs				;Yep, so leave
				lda	#00
				sta	objst,X		;Kill this object
				beq	loop2
			endif
			lda	#shtsz
			sta	temp4			;X in temp4, Y in 4+1
			sta	temp4+1		;Save size of shot for collision
			lda	objst,X		;See if this is a line
			and	#$10			;Line Flag
			ifne				;yep
				lda	#$60
				sta	temp4		;Much Wider
			endif
			jsr	hitship		;See if we hit ship??
			ifmi
				lda	#00
				sta	objst,X		;Kill the shot
				beq	loop2			;And skip draw
			endif
			jsr	dfshot		;Draw a fireball shot
loop2			dec	temp9
			ldx	temp9
			cpx	#zshot		;Last one??
		ccend
exit		rts	
		
trkspd	.byte $07,$0A,$10,$16,$1E,$26,$2E,$36,$3E,$46,$4E		;Tracking Speed

;************************************************
	.sbttl "Shot Utilities" 
;************************************************
;* Add A to x,velxl and x,velxh                 *
;* If adding a + velocity to a - or a - to a +, *
;* then will add velocity in twice.             *
;************************************************
xvlim	= $30		;X Velocity Limit

addvel		sta	temp1			;Save Velocity	
		sty	temp1+1		;Save MSB
		asl	temp1
		rol	temp1+1
		asl	temp1
		rol	temp1+1
		lda	#00
		sta	temp2			;Guess 1 pass
		lda	velxh,X
		eor	temp1+1		;Same direction already??
		ifmi				;no
			inc	temp2			;Do this twice
		endif
		begin
			lda	temp1
			clc
			adc	velxl,X
			sta	temp3
			lda	temp1+1
			adc	velxh,X
			ifpl
				cmp	#xvlim		;Don't let get too fast
				bcs	?ave10		;Skip if too big
			else
				cmp	#-xvlim	
				bcc	?ave10		;Skip if too big
			endif
			sta	velxh,X
			lda	temp3
			sta	velxl,X
			dec	temp2			;Double add if needed
		miend
?ave10	rts

;**************************************************
	.sbttl "Draw Fire Ball Shot"              
;**************************************************
; Called above and from TWStar                    *
;**************************************************	
dfshot		lda	#00
		jsr	drawshot		;This will position for us
		bmi	exit			;Skip if it did not place
		lda	#$EB			;Fixed intensity for now
		ldx	temp9
		ldy	objst,X		;Line or Shot??
		cpy	#$10
		ifcc
			eor	#02
		endif
		ldx	#smtb7		;Do a fireball or smart bomb
		jsr	vgadd2
		ldx	temp9
		lda	objst,X		;Fighter Shot??
		and	#$10			;Line bit set by Star Fort
		ifeq
?dfb10		lda	frame
			and	#06
			tay
			lda	smtb,Y
			ldx	smtb+1,Y
		else				;Space Wave 3... Angle Line
			lda	temp1			;Save Intensity
			tay
			lda	objxl,X		;Shot's X LSB
			sec
			sbc	shipxl		;Will angle about player
			sta	temp1
			lda	objxh,X
			sbc	shipxh
			sta	temp1+1
			ifcc				;Need to negate??
				jsr	dblneg		;Abs
				ldx	#$64			;And Flip other direction
			else
				ldx	#$60
			endif
			lda	#$EB			;Constant Intensity
			jsr	vgadd2		;Add this flip
			ldy	#02
			begin
				lsr	temp1+1
				ror	temp1
				dey	
			miend
			lda	temp1
			ldx	#00
			jsr	vgadd2		;Position to end of line
			lda	#$80			;Above Y, now do X
			ldx	#00
			jsr	vgadd2		;Always same length
			asl	temp1			;Back twice length
			rol	temp1+1
			jsr	dblneg		;Drawing Y the other way
			lda	temp1+1
			ifeq
				tax				;0 special case
			else_eq
				and	#$1F
				tax
				lda	temp1
			endif
			jsr	vgadd2
			lda	#00
			ldx	#$3F			;X always the same again
		endif
		jmp	vgadd2
		
;***********************************************
	.sbttl "Player Ship Collision Check"
;***********************************************
;* Checks an object against ship to see if it  *
;* has hit the ship. Returns - In status as    *
;* as setting the explosion flag in the ships  *
;* status word.                                *
;*                                             *
;* Inputs:	xcomp(4) = objects X & Y position  *
;*		(X) = object's index into obj**    *
;*		temp4 = Size of incoming object    *
;*          mzgame - skip if computer moving   *
;*		shipst - skip if already exploding *
;*                                             *
;* Uses:	temp1(word),temp2(word)	           *
;***********************************************
hitship	lda	shipst		;Ship Exploding??
		ifne
			ifpl				;nope
				lda	mzgame		;Computer Move??
				and	#$10
				ifeq				;skip if yes
					lda	#00
					sta	temp2+1		;2 numbers to add each less than 100
					lda	#shpszy		
					clc
					adc	temp4+1		;Y Size of object
					sta	temp2			;Y Size
					ifcs
						inc	temp2+1
					endif			;temp2 has Y 'collision size'
					lda	shipyl
					sec
					sbc	xcomp+2		;Shot's Y LSB
					sta	temp1
					lda	shipyh
					sbc	xcomp+3		;Shot's Y MSB
					sta	temp1+1
					bmi	?hs10			;Skip if past us
					lda	temp1
					cmp	temp2
					lda	temp1+1
					sbc	temp2+1		;Did we hit??
					ifcc				;We hit in Y, now check X
						lda	#00
						sta	temp2
						lda	#shpszx		;Ship Size in X
						clc
						adc	temp4			;X Size on collision object
						sta	temp2
						ifcs
							inc	temp2+1
						endif			;Total Size in temp2
						lda	shipxl
						sec
						sbc	xcomp			;Object's X LSB
						sta	temp1
						lda	shipxh
						sbc	xcomp+1		;Object's X MSB
						sta	temp1+1
						ifmi
							jsr	dblneg		;Abs of Y difference
						endif
						lda	temp1
						cmp	temp2
						lda	temp1+1
						sbc	temp2+1
						ifcc				;BOOM!!!!!
							jsr	domzsn
							jsr	blowship
							lda	#$80
							rts
						endif
					endif
				endif
			endif
		endif
?hs10		lda	#00
		rts
	
;************************************************
	.sbttl "Drop Shot From Enemy"
;************************************************
;* Check to see if time for space ship to shoot *
;*                                              *
;* Input:	(X) = space ship shooting           *
;************************************************		
dropshot	lda	frame
		and	#$07
		ifne
			rts
		endif
		lda	sobjyh,X
		cmp	#09			;Don't allow to shoot anymore if this is low
		ifcc
			lda	#$80
			sta	temp5			;Set from fighter flag
			;---------------------------------------------------------
			;------ Drop2: Entry from Star Castle to Start a Shot ----
			;---------------------------------------------------------
drop2			jsr	shotlimit
drop3			tay				;Get limits on how many
			iny				;Compensate for dey on entry to loop
			begin
				dey
				bit	temp5
				ifpl
					cpy	#zshot+nmshot-3		;Fort Steals top 3
				else
					cpy	#zshot			;Last one??
				endif
				ifcc
?ds10				lda	#$80				;Signal no shot
					rts					;None Available
				endif
				lda	objst,Y
			eqend				;Found an open one
			lda	#snd_e2
			jsr	dosound
			lda	sobjs2,X			;Moving??
			and	#$64				;Not moving, floowing, moving to top, skip
			bne	?ds10				;Skip if true
			lda	sobjxl,X			;Start at object
			sta	objxl,Y
			lda	sobjxh,X
			sta	objxh,Y
			lda	sobjyh,X
			sta	objyh,Y
			lda	sobjyl,X			
			sta	objyl,Y			;Position shot at ship
			;Shot must start out in direction fighter is pointing
			txa	
			pha					;Save this index, we will need it later
			lda	sobdir,X
			and	#$0F
			tax					;This guy's direction
			ifmi
				ldx	#08				;If 0, use straight down
			endif
			lda	cortlx-1,X			;Shot direction
			sta	velxh,Y
			lda	cortly-1,X
			asl	a				;Speed * 2
			sta	velyh,Y
			;If called by Starcastle, it will change velocities back later
			pla	
			tax				;Restore X (guy shooting)
			lda	#01
			sta	objst,Y		;Activate it!!
		endif
		rts
		
;*************************************************
	.sbttl "Shot Limit"
;*************************************************
;* Returns in A (zshot=nmshot) allowed this wave *
;* To use in most loops, this value must be      *
;* knocked down by 1.                            *
;*************************************************	
shotlimit	lda	difcty		;Add a shot each wave
		cmp	#02
		ifcs
			lda	#01
		endif
		clc	
		adc	#zshot+2
		cmp	#nmshot+zshot-3		;Recall we may steal 3
		ifcs
			lda	#zshot+nmshot-3		;Shot high limit
		endif
		rts
		
;******************************************
	.sbttl "Follow the Leader Routine"
;******************************************
followme	lda	temp3			;This is who we follow
		and	#$0F
		tay				;Index to other guy's position
		lda	sobjst,Y		;Is this guy blowing up?
		ifmi
?fm1			lda	#$10
			sta	sobjs2,X		;Turn Kamakazi
			rts
		else
			and	#$20			;Return bit
			bne	?fm1			;If yes, stop following
		endif
		sty	temp5			;See if following self
		cpx	temp5			;Following Self?
		beq	?fm1			;Shit!! How did that happen??
		lda	sobjxl,Y
		sta	temp4
		lda	sobjxh,Y		;Copy this away so we can reuse Y
		sta	temp4+1
		lda	sobjyl,Y
		sta	temp5
		lda	sobjyh,Y
		sta	temp5+1		;Now have copy of leaders position
		lda	sobdir,Y		;Leaders Direction
		and	#$0F
		tay				;Will use this as index to 'pole' table
		lda	sobjs2,X		;See if a Left or Right guy
		and	#$08			;Left/Right bit
		ifne
			tya
			clc
			adc	#$10			;Use upper half of table
			tay
		endif	
		lda	temp4			;Update X
		clc	
		adc	rodxl,Y
		sta	temp6
		lda	temp4+1
		adc	rodxh,Y
		sta	temp6+1		;New X Position Target
		lda	temp5
		clc	
		adc	rodyl,Y
		sta	temp7
		lda	temp5+1
		adc	rodyh,Y
		sta	temp7+1		;New Y Position Target
		tya	
		sta	sobdir,X		;And set picture
		lda	#$28
		sta	temp8
		sta	temp8+1		;Target Speeds
		jmp	approach
		
;*******************************************
;* Table of rod lengths for each direction *
;* Bytes 0-15 are for left side, 16-31 for *
;* right. Recall 0 is not sued so a dummy  *
;* byte will hold it's place.              *
;*******************************************
z 	=  7		;Maximum is 7

rodxl		.byte	$00,$d2,$E0,$D2,$9A,$54,$00,$AC
		.byte $66,$2E,$20,$2E,$66,$AC,$00,$54
		.byte $00,$AC,$00,$54,$9A,$D2,$E0,$D2
		.byte $9A,$54,$00,$AC,$66,$2E,$20,$2E
		
rodxh		.byte $00,$00,$00,$00,$00,$00,$00,$FF
		.byte $FF,$FF,$FF,$FF,$FF,$FF,$00,$00
		.byte $00,$FF,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$FF,$FF,$FF,$FF,$FF
		
rodyl		.byte $00,$2A,$00,$D6,$B3,$97,$90,$97
		.byte $B3,$D6,$00,$2A,$4D,$69,$70,$69
		.byte $00,$69,$70,$69,$4D,$2A,$00,$D6
		.byte $B3,$97,$90,$97,$B3,$D6,$00,$2A
		
rodyh		.byte $00,$00,$00,$FF,$FF,$FF,$FF,$FF
		.byte $FF,$FF,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$FF
		.byte $FF,$FF,$FF,$FF,$FF,$FF,$00,$00

;*************************************************
	.sbttl "Kamakazi Control"
;*************************************************
;* Called if an object is to kamakazi the player *
;*                                               *
;* Inputs: 	Bit D4=1 In sobjs2                   *
;*		    D3=1 If only allowed clockwise   *
;*              Else 0= only allowed c-clockwise *
;*************************************************
kama		lda	frame
		and	#$03
		ifeq
?kc10			lda	shipxh
			sec	
			sbc	sobjxh,X
			clc	
			adc	#07
			tay	
			lda	sobdir,X		;Where we are pointing
			and	#$0F
			sec	
			sbc	tardir,Y		;Want to point that way?
			ifne
				ifpl
					dec	sobdir,X
				else
					inc	sobdir,X		;Point about
				endif
			endif
		endif
		lda	sobdir,X
		and	#$0F			;Don't allow to point 0
		beq	?kc10			;So bump it again
		rts	
		
tardir	.byte $04,$05,$05,$06,$06,$06,$06
		.byte $08,$0A,$0A,$0A,$0B,$0B,$0C
		
;********************************************************
	.sbttl "Line Up for Start of Coreg"
;********************************************************
;* Called from 'coreg' with X pointing at proper object *
;********************************************************
txvel	= $30
tyvel = $30

setup		lda	f_targxl,X
		sta	temp6
		lda	f_targxh,X
		sta	temp6+1		;Put in temp6 for target routine
		lda	f_targyl,X
		sta	temp7
		lda	f_targyh,X
		sta	temp7+1		;X and Y position current
		lda	#txvel		
		sta	temp8			;Closing velocity X
		lda	#tyvel
		sta	temp8+1		;Closing velocity Y
		jsr	approach
		ifmi				;We are there
			lda	sobjst,X
			and	#$BF			;Drop Setup Bit
			sta	sobjst,X
			lda	sobjs2,X
			ora	#$40			;Turn on hold in pattern
			sta	sobjs2,X
			ldy	sobjxh,X		;Get X Position
			lda	#$10
			sta	sobdir,X		;Face Down
			dec	wtcnt
		endif
		rts				;Done
		
f_targxl		.byte $00,$00,$00,$00,$00,$00,$80,$80,$80,$00,$00,$80,$80

f_targxh		.byte $02,$03,$04,$05,$06,$07,$03,$04,$05,$04,$05,$04,$04

f_targyl		.byte $80,$80,$80,$80,$80,$80,$00,$00,$00,$80,$80,$00,$80

f_targyh		.byte $02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03,$04,$04

;*******************************************************
	.sbttl "Approach Target Routine"
;*******************************************************
;* Will approach the target given in temp6(X) and      *
;* temp7(Y) (2 bytes each) at velocities given in      *
;* temp8(X) and temp8+1(Y). If the difference to target*
;* is less than the velocity, the object is simply     *
;* placed at the target position.                      *
;*                                                     *
;* Uses:  temp1,temp2,temp6,temp7,temp8                *
;*                                                     *
;* Returns minus if at target, else +                  *
;*******************************************************
approach	ldy	#00
		sec	
		lda	sobjxl,X		;Distance to target
		sbc	temp6
		sta	temp1
		lda	sobjxh,X
		sbc	temp6+1
		sta	temp1+1
		pha				;Save the 'real' distance
		ifmi				;To the left??
			jsr	dblneg
		endif
		ifeq				;MSB is close
			lsr	temp8			;Use half velocity
			lda	temp1
			cmp	temp8			;Less than velocity to target??
			ifcc				;If yes, just put at target
				pla			;Throw away pha
				lda	temp6
				sta	sobjxl,X
				lda	temp6+1		;Just put at target
				sta	sobjxh,X
				ldy	#$80
				bne	?ap10			;Say at target
			endif
			cmp	#$80
			ifcc				;Really close
				lsr	temp8
			endif
		endif			;Not Close
		pla				;Recall 'real' A
		ifpl
			dey
			lda	#00
			sec
			sbc	temp8			;Velocity is minus here
		else
			lda	temp8
		endif
		clc	
		adc	sobjxl,X
		sta	sobjxl,X
		tya	
		adc	sobjxh,X
		sta	sobjxh,X		;Get to X target
		iny				;Insure Y is +
?ap10		sty	temp2			;Store signal here
		ldy	#00			;Guess Y not at target
		sec	
		lda	sobjyl,X		;Distance to target
		sbc	temp7
		sta	temp1
		lda	sobjyh,X
		sbc	temp7+1
		sta	temp1+1
		pha				;Save the 'real' distance
		ifmi				;To the left?
			jsr	dblneg
		endif
		ifeq				;MSB is close
			lsr	temp8+1
			lda	temp1
			cmp	temp8+1		;Less than velocity to target??
			ifcc				;If yes, just put at target
				pla				;Throw away pha	
				lda	temp7
				sta	sobjyl,X
				lda	temp7+1		;Just put at target
				sta	sobjyh,X
				ldy	#$80
				bne	?ap20			;Say at target
			endif
			cmp	#$80
			ifcc
				lsr	temp8
			endif
		endif			;Not Close
		pla				;Recall 'real' A
		ifpl
			dey
			lda	#00
			sec
			sbc	temp8+1		;Get minus velocity
		else
			lda	temp8+1
		endif
		clc	
		adc	sobjyl,X
		sta	sobjyl,X
		tya	
		adc	sobjyh,X
		sta	sobjyh,X		;Get X target
		iny				;Insure Y is +
?ap20		tya	
		and	temp2			;Total Signal
		rts
		
;*************************************************
	.sbttl "Past Bottom Check"
;*************************************************	
pastck	ifpl	
			cmp	#$0B		;Hit the bottom??
			ifcs			;yep, at bottom
				jsr	stcorg
				ora	#$20		;Force back up bit
				sta	sobjst,X
				lda	sobjs2,X
				and	#$C0		;Drop all but set-up and bonus bits
				sta	sobjs2,X
				sed			;*********** Caution ***************	
				sec	
				lda	bonusa	;Dec Bonus
				sbc	#04		;-400 pts
				beq	?pb10		;Count this one too
				ifmi			;Don't let wrap
?pb10					lda	sobjs2,X		
					ora	#$80		;Set once past bit
					sta	sobjs2,X
					lda	#00
				endif
				sta	bonusa
				cld			;***********************************
				lda	#-1
				sta	sobjxl,X	;Guess on right (4.FF)
				ldy	#rtedge-1	;Guess right side
				lda	sobjxh,X	;Which side are we on??
				cmp	#center
				ifcc
					ldy	#00		;Nope, put on the left edge
				endif
				tya	
				sta	sobjxh,X	;Force to Edge
				ifeq			;Is on the left, so store LSB to 0 too
					sta	sobjxl,X
				endif
				ldy	#$11		;Guess off right edge
				lda	sobjxh,X
				ifeq			;It was left
					ldy	#$1F		;Point for left
				endif
				tya	
				sta	sobdir,X	;Point Correctly
				lda	#$0A		;At bottom of screen
				sta	sobjyh,X
				lda	#-1
				sta	sobjyl,X
			endif
		endif
		rts
		
;********************************************
	.sbttl "Start (restart) Correography"
;********************************************	
stcorg	lda	sobjst,X
		ora	#$10			;Make sure it is on
		and	#$1F			;Drop all but on and coreg number
		sta	sobjst,X
		pha	
		and	#$0F			;Coreg
		tay				;Save copy
		lda	coegts,Y		;Table Pointers
		sta	sobcog,X		;Start this coreg
		;Want to return with A=current sobjst
		pla	
		rts	
		
;********************************************
	.sbttl "Get Empty Slot"
;********************************************
;* Find an empty slot.... Return minus set  *
;* if available, else return zero. If zero, *
;* X points to empty slot                   *
;********************************************		
getempty	ldx	#nmform		;Find an empty one
		begin	
			dex
			bmi	?ge10
			ifeq				;First Guy??
				lda	maznum		;Doing Space Fort??
				cmp	#spacefort		
				ifeq
					lda	#$80			;Don't use this one
					bmi	?ge10			;And just leave
				endif
			endif
			lda	sobjst,X
		eqend			;Got one!
		lda	#00
?ge10		rts

;**********************************************
	.sbttl "Skip Rest of Space?"
;**********************************************
;* This routine decides if it is time to skip *
;* the rest of the fighter wave and go on to  *
;* the docking wave.                          *
;* Necessary Conditions: All active fighters  *
;* must be either returning to the top or     *
;* just getting ready to return, the base star*
;* must be halfway down the screen.           *
;**********************************************	
skip		lda	statyh
		bpl	?s10			;Skip if off screen
		cmp	#07
		bcs	?s10			;Still too high on screen
		ldx	#nmform-1
		ldy	#00
		begin
			lda	sobjst,X
			ifne
				ifpl				;Active and not exploding??
					and	#$20			;Waiting to go up??
					ifeq				;Here is one not ready
?s10						rts				;Not all ready
					endif
				endif
			endif
			dex
		miend
		lda	lasst			;Laser shot out??
		bne	?s10			;Then wait
		jmp	noneleft		;Force to next level

;************************************************
	.sbttl "One Less Enemy to Deal With"
;************************************************
oneless	dec	nenemy		;One less to fight
		ifeq				;We are done
			lda	nenstr		;Any left to start??
			ora	mzgame		;During space that is
			ifeq				;nope
noneleft			jsr	dostop		;Force to next wave
				lda	#$10
				sta	mzgame		;Next Game
				lda	#$40
				sta	lauen			;Skip Launch next time
				lda	#$80
				sta	stbflg		;Set Flag for down screen
				lda	bonusa		;Xfer bonus to display buffer
				sta	scbdis+1
				lda	#00
				sta	scbdis
				lda	#$0F
				sta	scbdis+3
				lda	#04
				sta	spcspd		;Speed of stars
				lda	statyh		;Already moving??
				ifeq				;nope, can restart
					lda	#$FF
					sta	statst		;Turn on the station
				endif
				lda	#$0B
				sta	shipyh
				lda	#$B8
				sta	shipyl		;Correct position for 3rd person display
				lda	manstat		;We alive??
				ifne
					ifpl				;yep!
						lda	maznum
						lsr	A			;On 1 and 3 of 0,1,3
						ifcs
							lda	#snd_mys
							jmp	dosound
						endif
					endif
				endif
			endif
		endif
		rts
		
;**************************************************
	.sbttl "Next Entry"
;**************************************************
;* Gets next entry in coreg table for this guy.   *
;*                                                *
;* Entry: (X) = Object Index                      *
;**************************************************	
nextent	inc	sobcog,X		;Next Entry
thisent	ldy	sobcog,X		;Entry to get current entry index
?ne10		lda	coegrt,Y		;Tabel of moves
		cmp	#$80			;End of lists??
		ifeq
			lda	sobjst,X		;Which pattern??
			and	#03
			tay	
			lda	coegts,Y		;Start of this coreg
			sta	sobcog,X		;Reset Entry
			tay				;And return with...
			lda	coegrt,Y		;A = new dance
		endif
		tay	
		lda	sobjst,X		;Check for which edge
		and	#$0F			;Top 4 are left
		cmp	#04			;Set carry for test
		tya				;Recall new direction and time
		ifcs				;This one of top 4
			sta	temp2			;Save this
			and	#$0F			;Current Entry - Time
			tay				;Get index to lfttbl
			lda	lfttbl,Y		;Get Left direction
			eor	temp2
			and	#$0F
			eor	temp2			;Replace Time
		endif
		rts	
		
;***************************************************
	.sbttl "Copy Position"
;***************************************************
;* Copies the enemy fighters quads into xcomp for  *
;* the display routine. Called when the fighter is *
;* not moving (for whatever reason).               *
;***************************************************
copypos	lda	sobjxl,X
		sta	xcomp
		lda	sobjxh,X
		sta	xcomp+1
		lda	sobjyl,X
		sta	xcomp+2
		lda	sobjyh,X
		sta	xcomp+3
		rts	
		
;*****************************************************
	.sbttl "Start from Rack"
;*****************************************************
;* This routine is used to start fighters from their *
;* holding positions in the 'rack'.                  *
;*                                                   *
;* Uses:	temp1,temp2,A,X,Y,nxtptr,nxtdly          *
;* Inputs:	frame                                    *
;*****************************************************
next		jsr	snake			;Continue Snake if needed
		bit	toolong		;Stalling??
		bmi	?sr5			;No Check if stalling
		ldy	#00			;Count moving ones
		ldx	#nmform-1
		begin
			lda	sobjst,X		;Don't count active or blowing up
			ifne
				ifpl
					lda	sobjs2,X		;Holding in place
					and	#$40
					ifeq
						iny
					endif
				endif
			endif
			dex
		miend
		;Now Y tells us how many are in space
		cpy	#07
		ifcc				;Hold at 6 or 7
?sr5			bit	nxtskp		;Did the last one start??
			bmi	next2			;If no, force start
			lda	frame
			and	#01
			ifeq				;Time for next check
				dec	nxtdly		;Time for next start??
				ifmi				;yep
next2					lda	#00
					sta	nxtskp		;Guess we will start something
					ldx	nxtptr
					lda	nxttbl,X		;Get next table entry
					ifeq
						tax
						inx
						stx	nxtptr		;Restart with start of table
						lda	nxttbl,X		;Need to get the entry now
					endif
					inc	nxtptr		;Bump pointer
					pha
					and	#$0F			;Get who
					tax				;Save for routine
					pla				;Get back what
					and	#$F0			;Only top nibble
					lsr	A
					lsr	A
					lsr	A
					tay				;Do Case
					lda	nxtcse+1,Y		;Get Case
					pha
					lda	nxtcse,Y
					pha
					ldy	nxtptr		;So routine will have this too
					lda	nxttbl,Y		;And this too!
				endif
			endif
		endif
		rts				;Do Case!	


nxtcse	.word cse0-1
		.word cse1-1
		.word cse2-1
		.word cse3-1
		
nxexit	inc	nxtptr		;Look at next entry	
nxexit2	ldx	nxtptr
		inc	nxtptr
		lda	nxttbl,X		;Get Next Entry
		ifeq				;Get Next Entry and start right now
			bne	next2			;****** Always ******
		endif
		;If not X, then it's a delay till next time
		sta	nxtdly
		rts
		
;*********************************************
	.sbttl "Cases"
;*********************************************
;* Entry Y = nxttbl pointer to current entry *
;* 	   X = pointer to object to be started *
;*       A = nxttbl entry value              *
;*********************************************
;* Case 0 - Start Correography               *
;*********************************************	
cse0		jsr	movchk		;Already moving??
		ifne				;no, this is okay
			jsr	cse0r			;The routine is a subroutine so other 
							;parts of these cases may use it
		endif
		jsr	stchk			;See if started??
		jmp	nxexit		;Do Exit!
		
cse0r		jsr	putinc		;Put coreg bits in lower nibble
		lda	#04			;Turn on go to top bit
		jmp	putin			;Put in these bits and return

;*********************************
;* Case 1 - Start a Kamakazi     *
;*********************************
cse1		jsr	movchk		;Already Moving??
		ifne				;This okay to start
			lda	#$10			;Kamakazi Flag
			jsr	putin			;Turn on correct bits
		endif
		jsr	stchk			;See if started
		jmp	nxexit2		;And Leave..

;***************************		
;* Case 2 - Start a Snake  *
;***************************
cse2		bit	snakef		;Already trying to start??
		ifpl				;nope
			sta	snakec		;Save snake count and coreg
			jsr	getrand
			and	#04
			eor	snakec
			sta	snakec
			lda	#$80
			sta	snakef		;Turn of flag
			stx	snakex
		endif
		jmp	nxexit		;End of exit will start first of snake

;***************************************************
;* Utility to OR in A to sobjs2 then drop hold bit *
;***************************************************
putin		ora	sobjs2,X		;Put in new bits
		and	#$BF			;Drop hold bit
		sta	sobjs2,X
		rts	

;************************************************************** 
;* Utility to put in coreg in A into bottom nibble of sobjst  *
;**************************************************************
putinc	eor	sobjst,X		;Replace coreg
		and	#$0F			;Replace bottom nibble which is coreg bits
		eor	sobjst,X
		bmi	?pi10			;If it was blowing up, skip it
		bit	m10			;See if previously active
		ifne				;It was
			sta	sobjst,X		;Replace (but do not turn on if dead already)
		endif
?pi10		rts	

m10		.byte $10

;*********************************************
;* Utility to see if we started anybody.     *
;* Assumes X points to object just trying to *
;* start.                                    *
;*********************************************
stchk		lda 	sobjst,X		;Tried to start this guy
		bmi	?stc10		;If blowing up, he didn't start
		and	#$10			;Alive??
		ifeq				;No, he didn't start
?stc10		lda	#$80			;Set bad start
			sta	nxtskp
		endif
		rts
		
;***************************************
;* Check to see if already moving and  *
;* don't restart                       *
;***************************************	
movchk	lda	sobjs2,X		;Check hold bit
		and	#$40			;If ne on return, holding
		rts	
		
;***************************************
;* Case 3 - Start Fighter Squad        *
;***************************************
cse3		pha				;Save what coreg
		txa				;A is who we want to follow
		sta	temp1			;Save who to follow for routine below
		inc	nxtptr		;Will need the next entry
		iny				;And the copy of it
		jsr	movchk		;Leader waiting??
		ifeq				;No, already moving (maybe)
			pla			;Throw away this
		else				;Else, start him
			pla				;Start leader
			and	#$0F			;Pass coreg to routine 0
			jsr	cse0r			;Start the first guy as normal
			lda	sobjst,X		;See if active
			and	#$10			;Skip if dead
			ifne	
				lda	nxttbl,Y		;Who L and who R
				pha 				;Save copy
				and	#$0F			;Who on the right??
				tax				;Who we will change
				lda	temp1			;Who we will follow
				jsr	putinc		;Put this in correography as who to follow
				lda	#$20
				jsr	putin			;Add follow the leader bits
				pla	
				lsr	A
				lsr	A
				lsr	A
				lsr	A			;Now get who R
				tax				;Set current to this guy
				lda	temp1			;Who we will follow
				jsr	putinc
				lda	#$28
				jsr	putin
			endif
		endif
		jmp	nxexit		;And Leave
		
;************************************
	.sbttl "Snake Routine"
;************************************
snake		lda	snakef		;Snake trying to start??
		ifmi				;yes
			lda	frame
			and	#07			;Time??
			ifeq				;yes
				ldx	snakex		;Who we will start this time
				jsr	movchk		;Make sure he is waiting
				ifne
					lda	snakec		;Get coreg
					and	#$0F
					jsr	cse0r			;Start this guy
				endif
				inc	snakex		;Point to possible next
				lda	snakec
				sec
				sbc	#$10			;Count = Count -1
				sta	snakec
				ifmi
					lda	#00
					sta	snakef		;Done
				endif
			endif
		endif
		rts	
		
;****************************************
	.sbttl "Start From Rack Table"
;****************************************
nxttbl	.byte $01,$3C,$03,$AB		;Start Squad, Coreg 3, #0C Lead, A & B Follow
		.byte $07,$20,$52			;Start Snake using #0, 6 Guys, Coreg 2
		.byte $07,$07,$00			;Start Simple, #7, Coreg 0
		.byte $07,$08,$01			;Start Simple, #8, Coreg 1
		.byte $07,$1B			;Kamakazi #B (if start above)
		.byte $07,$33,$01,$24		;Squad, Coreg 1,2&4 Follow 3
		.byte $03,$19			;Kamakazi #7
		.byte $03,$1A			;Kamakazi #A
		.byte $0F,$27,$23			;Start Snake, #7, 3 Guys, Coreg 3
		.byte $03,$1B
		.byte $00,$17
		.byte $00,$16
		.byte $00,$00
		
;****************************************
	.sbttl "Correography Table"       
;****************************************
; Simple Right Sweep
coegrt
___1		.byte $50,$4B,$3A,$49,$98,$87,$C6,$F5,$80

; Right Sweep with loop back
___2		.byte $50,$7A,$79,$88,$57,$46,$35,$24	;S Loop
		.byte $13,$12,$11,$1F,$1E,$1D,$1E
		.byte $1F,$11,$12,$13,$14,$15,$16
		.byte $17,$18,$F9,$80
		
; Switch Back(Zig/Zag)
___3		.byte $50,$5B,$3A,$39,$28,$17,$26
		.byte $35,$94,$15,$16,$17,$18,$19
		.byte $1A,$1B,$BC,$1B,$1A,$19,$18
		.byte $17,$16,$15,$74,$15,$16,$17
		.byte $18,$19,$FA,$80
		
; Loop De Loop
___4		.byte $A9,$18,$17,$16,$15,$14,$13
		.byte $12,$11,$1F,$1E,$1D,$1C,$1B
		.byte $1A,$19,$18,$A7,$18,$19,$1A
		.byte $1B,$1C,$1D,$1E,$1F,$11,$12
		.byte $13,$14,$15,$16,$17,$18,$80
		
; Table of offsets to get a coreography
coegts	.byte ___1-coegrt
		.byte ___2-coegrt
		.byte ___3-coegrt
		.byte ___4-coegrt
		.byte ___1-coegrt
		.byte ___2-coegrt
		.byte ___3-coegrt
		.byte ___4-coegrt

; Table of replacment values to change from right to left side
lfttbl	.byte $00,$0F,$0E,$0D,$0C,$0B,$0A,$09,$08,$07,$06,$05,$04,$03,$02,$01

;******************************************
	.sbttl "Table of Delta X and Y"
;******************************************
;* Table of Delta X and Delta Y for a     *
;* given angle.                           *
;******************************************
cortlx	.byte $F4,$EA,$E2,$E0,$E2,$EA,$F4,$00,$0C,$16,$1E,$20,$1E,$16,$0C
cortly	.byte $F4,$F8,$FD,$03,$09,$0E,$12,$13,$12,$0E,$09,$03,$FD,$F8,$F4

;******************************************
	.sbttl "Draw Enemy"
;******************************************
;* Draw an Enemy Fighter                  *
;*                                        *
;* Inputs: xcomp(4)= position             *
;******************************************
drawen	lda	#00
		sta	scalef		;fract
		lda	#$FE
		sta	scalef+1		;Binary Offset in Size
drawen2	;Entry from blowit
		lda	temp3			;Blowing Up??
		ifmi				;Yeah, so get larger
			asl	a
			asl	a
			sta	scalef
			ifcs
				inc	scalef+1
			endif
		endif
		lda	mzgame
		bmi	?de10			;Special for inside maze blow up
		cmp	#01
		ifeq
?de10			jsr	cor3p
			lda	#00			;Set minus
		else
			jsr	posvec		;Position and set scale of object
		endif
		ifpl				;Only do this if on screen
			ldy	temp9
			lda	sobjst,Y
			and	#03
			tax
			lda	enmcol,X		;Color is coreg
			ldx	#enm7			;Stat, page select
			bit	temp3			;Exploding?
			ifmi
				lda	temp3			;Dim it out
				cmp	#$D0			;Bright then dim
				ifcc
					lda	#$FA			;Bright Red
				else
					eor	#$FF
					and	#$F0
					clc	
					adc	#$62
				endif
				ldx	#fexps7+sparkle+xflip		;Stat, PageSel, Sparkle, Xfliip
			else
				ifvs				;Setting up?
					lda	#$E4			;Move in in Red
				endif
			endif
			pha				;Save color
			ldy	temp9			;Recall X
			lda	sobdir,Y		;Direction
			and	#$0F			;Drop timer
			cmp	#09
			ifcs				;Will need to flip these
				pha
				txa				;Need to flip??
				ora	#04			;Turn it on
				tax
				pla
			endif
			tay
			stx	temp1+1		;Save Xflip info
			lda	pictabl,Y		;Get the picture??
			sta	temp1			;Hold this
			pla				;Recall Color
			sta	temp2			;Save Color
			jsr	vgadd2		;Add Stat
			lda	temp3			;Blowing up??
			ifmi				;yep
				and	#$F0
				lsr	A
				lsr	A
				lsr	A
				sec
				sbc	#$10			;This number goes from 8 to F
				pha				;Save Y index
				jsr	addpic
				lda	temp1+1		;Do other half of pic
				eor	#xflip		;Flip other way
				tax
				lda	temp2
				jsr	vgadd2		;Flip and re-add color
				pla
				jsr	addpic
			else
				ldy	temp1
				lda	enemys,Y
				ldx	enemys+1,Y
				jmp	vgadd2		;Output picture
			endif
		endif
		rts
				
addpic	tay	
		lda	fexps,Y
		ldx	fexps+1,Y		;Get Figher Pic
		jmp	vgadd2
		
pictabl	.byte $0E,$00,$02,$04,$06,$08,$0A,$0C,$0E,$0C,$0A,$08,$06,$04,$02,$00

enmcol	.byte $F1,$F2,$F7,$F6

;***********************************************
	.sbttl "Fighter to Shot Collision"
;***********************************************
;* X=Object Index, will check against all shots*
;* It is assumed that xcomp(4) has position.   *
;*                                             *
;* Entry hitwbs for web spinners               *
;*                                             *
;* Uses: temp1(word),temp4(word),temp6(word)   *
;*       5 bytes stack                         *
;***********************************************
shtchk	lda	#enszx		;Size of Fighter
		sta	temp4
		lda	#enszy		;Y Size
shtchk2	clc	
		adc	#04			;Shot needs a Y Size
		sta	temp4+1
		;Temp4 = Size of Object
hitwebs	lda	sobjst,X		;Make sure not exploding
		ifmi
?fs1			rts
		endif
		beq	?fs1			;Skip if dead too!
		ldy	#nmsshots-1
		begin
			lda	shotst,Y		;Shot Active
			ifne				;yep
				ifpl
					lda	xcomp			;Stored XL
					sec
					sbc	shotxl,Y
					sta	temp1
					lda	xcomp+1
					sbc	shotxh,Y
					sta	temp1+1
					ifmi
						jsr	dblneg		;Positive
					endif
					ifeq				;MSB is 0
						sec
						lda	temp1
						sbc	temp4			;X Difference to Size
						ifcc
							lda	shotyl,Y
							sec
							sbc	xcomp+2
							sta	temp1
							lda	shotyh,Y
							sbc	xcomp+3
							sta	temp1+1
							ifmi				;We are passed
								cmp	#$FF			;Skip if way passed
								ifeq				;Just passed
									lda	temp1			;See if we passed through
									sec
									sbc	#shtspd
									sta	temp6
									lda	temp1+1
									sbc	#00
									sta	temp6+1		;Old Position
									sec	
									lda	xcomp+2
									sbc	temp6
									lda	xcomp+3
									sbc	temp6+1		;If just passed, the will go +
									bmi	?fs5			;Skip this, already passed
									lda	xcomp+2
									sta	shotyl,Y
									lda	xcomp+3
									sta	shotyh,Y		;Put shot at target
									lda	#00
									sta	temp1			;Fake to say on target
								endif
							endif
							ifeq				;MSB must be 0
								sec
								lda	temp1
								sbc	temp4+1		;Size compare Y
								ifcc
									lda	#$C0
									sta	shotst,Y		;Turn off shot
									jsr	killit
								endif
							endif
						endif
					endif
				endif
			endif
?fs5			dey
		miend
;**********************************************
;* Now check object to plyaer ship collision  *
;**********************************************	
		lda	shipst		;Only if ship active
		ifmi				;Blowing Up?
?fs10			rts
		endif
		beq	?fs10			;Skip not active
		;xcomp still has this ships position
		lda	#enszy		;Y Size
		sta	temp4+1		;Recalculate - shell size above
		jsr	hitship
		ifmi				;We hit it
			jmp	killit		;Do what is needed!!
		endif
		rts
		
;**********************************************
	.sbttl "Destroy Enemy Fighter"
;**********************************************	
killit	lda	sobjst,X		;Is this one in staging??
		and	#$40
		ifne				;Must count it out then
			lda	wtcnt			;Don't let past 0
			ifne
				dec	wtcnt			;One less to wait
			endif
		endif
		lda	#$80
		ldy	maznum		;Make spinners wants a 4 here if hit
		cpy	#spacemaze
		ifeq
			lda	#05
		endif
		sta	sobjst,X		;And the ship we hit
		jsr	domzsn
		txa	
		pha				;Save X
		lda	hitpts		;Points
		jsr	bpont2
		pla	
		tax	
		rts	
		
domzsn	lda	maznum
		ifne
			cmp	#02
			ifeq
				lda	#snd_f2
			else
				lda	rands
				and	#01
				ifeq
					lda	#snd_e3
				else
					lda	#snd_e4
				endif
			endif
			jsr	dosound
		endif
		rts
			
mror		begin
			cmp	#$80
			ror	A
			ror	temp1
			dey
		miend
		rts	
		
		
;********************************************************
	.title "TWShip - Ship Control"
	.sbttl "Game States"
;********************************************************
;* Game States and the numbers that go with them..      *
;*                                                      *
;*     STATE                   MZGAME  GAMEST  X FOR    *
;*       #                     VALUE   BIT 7   TARGSHIP *
;*------------------------------------------------------*
;*                                                      *
;* 1). In Tube                 40         0    0/2      *
;* 2). Tact Dsp, Space Trans.  20         0     1       *
;* 3). Space Play			 00         0     2       *
;* 4). Left/Right Move(trans)  10         0    3/4      *
;* 5). Upwards trans to center 08         0     5       *
;* 6). Landing on Maze         04         0     2       *
;* 7). Zoom in after land      02         0     6       *
;* 8). Game Play in Maze       80        0/1   ***      *
;* 9). Zoom out after Exit     02         1     7       *
;*10). Take off from Maze      04		1     2       *
;*11). Blow Maze Sequence      01         1    9/10     *
;*                                                      *
;* Note:	Gamest bit 7 represents a succesful exit    *
;*          from the maze. X is the index to the        *
;* 		motion/speed used by that section of game   *
;*          play.                                       *
;* Note:  	If tact is minus, then tactile scanner mode *
;* 		is on; and X is always set to 8 until that  *
;*          mode ends. No change of gamest will take    *
;*     	place in the tact routine.                  *
;********************************************************
mvspdy	.byte $06,$16,$00,$28,$28,$28,$28,$18,$18,$04,$10	;Y Speed
mvspdx	.byte $02,$04,$00,$08,$08,$08,$08,$04,$08,$01,$01	;X Speed

targx		.byte center-1,center,0,rtedge-3				;X Target Position
		.byte center-1,center,center+1
		.byte center,center,center,center
		
targxl	.byte $00,$70,$00,$00,$00,$80,$33,$80,$80,$80,$80	;X Position LSB Target

targy		.byte $00,$0B,$00,$0B,$0B,$01,$05,$08,$08,$04,$06	;Y Target Position

targyl      .byte $00,$B8,$00,$B8,$B8,$00,$40,$D0,$20,$00,$00	;Y Position LSB Target

mvscal	.byte $01,$04,$00,$03,$03,$03,$06,$04,$08,$05,$08	;Scale Change Amount

tarscl	.byte $70,$16,$10,$16,$16,$28,$10,$28,$00,$10,$16	;Target Scale Linear

tarsch	.byte $71,$72,$71,$72,$72,$72,$70,$72,$73,$76,$70	;Target Scale Binary


;********************************************
	.sbttl "Transitions Routine"
;********************************************
;* Entry from Mainline only if mzgame is    *
;* not minus                                *
;********************************************
shipout	;Do Ship Routine
		;Select Target position, scale and speed index
		ldy	#00			;If 0 at end, we do targship
		ldx	#02			;If we fall out bottom, we are in space
		lda	mzgame		;Which Play Level
		ifeq				;In space
			ldx	spcspd		;Slow Down Space
			cpx	#06
			ifcc
				lda	frame
				and	#$1F
				ifeq
					inc	spcspd		;Slow Down
				endif
			endif
			dey			;Skip targship
		else
			asl	a		;40 into -
			ifmi				;Game Status 2 (40=In Tube)
				ldx	#00			;Tube Motion
				bit	tstat			;Tube Motion on Hold??
				ifpl				;Yes
					ldx	#02			;So do nothing now
				endif
			else				;Not 40
				asl	a	;20 into -
				ifmi			;Game State 3 (20 =Transition Down)
					jsr	pictran		;Need to do transition (maybe)
					ldx	#01
					bit	tact			;Doing Tact Scanner?
					ifmi
						ldx	#08			;Hold at center then
					endif
				else			;Not 40 or 20
					ldx	#$80
					stx	nosync		;Do this fast
					asl	a			;10 into -
					ifmi				;Game State 4 (10 = Left/Right Move)
						ldx	#03			;Will Guess Transition Right
						lda	shipxl
						cmp	#$80
						lda	shipxh		;Which Side are we on??
						sbc	#center		;Greater than 3,80??
						ifcc				;The Left!!
							ldx	#04			;Move Left instead
						endif
					else				Not 40,20 or 10
						asl	a			;8 into -
						ifmi				;Game State 5 (08 = Upwards Transition)
							jsr	pictran		;Change Pics!
							ldx	#05
						else				;Not 40,20,10 or8
							asl	a			;4 into -
							ifmi				;Game State 6 or 10
								ldx	#02
								dey				;Neither do anything
							else				;Not 40,20,10,08 or 04
								asl	a			;2 into -
								ifmi				;Game State 8 or 10 (Zooming!)
									bit	gamest		;In or Out?
									ifvc				;Zoom in
										ldx	#06
									else
										ldx	#07
									endif
								else				;Else must be State 1 (Blow Station)
									jsr	blowit		;Do Motion Control
									ldx	#09			;Do Zoom stuff
									lda	blowst		;Start moving forward
									ifeq
										ldx	#10d
									endif
								endif
							endif
						endif
					endif
				endif
			endif
		endif
		;******************* Fall Through to Next **********************
		tya				;Do Targship??
		ifpl
			jsr	targship
		endif
		jsr	moveship		;Move and Draw Ship
		;Above calls shipdis
		lda	shipst		;Skip if the ship died
		ifne
			ifpl
				lda	targf			;At target above??
				ifmi				;yes
					lda	mzgame		;First time in??
					ifpl				;Might have gone minus
						cmp	#$20
						ifcs				;20 or 40??
							bit	tact			;But wait for tact to go away
							ifpl
								jsr	startspace		;Start Space Wave
								lda	#$3F
								sta	mtim			;Display Message (If Needed)
								lda	#00
								sta	tact			;Clear Tact Display
								jsr	initcol		;Reset Colors
							endif
						else
							cmp	#08		;From Transition Mode??
							ifeq			;yep
								jsr	dospace
							else
								cmp	#01		;Blowship??
								ifeq			;yes
									lda	blowst		;Final Sequence?
									ifeq
										sta	seqst
										sta	seqx
										lda	#$20
										sta	mzgame		;Back to tact (downward)
										lda	#$80
										sta	gamest		;Clear Exit Bit
										sta	tactde		;Still Want tact next time
										sta	tact		;And turn this back on now
										jsr	nextmz		;Set up next maze
										lda	maznum
										sec
										sbc	#01
										and	#03
										sta	seqp			;Start 1 pic back
									else				;Could be moving back
										cmp	#$40			
										ifeq
											lda	#$50			;Set Move Station Bit
											sta	blowst
											lda	#$80
											sta	statst		;And Turn on Station
										endif
									endif
								endif
							endif
						endif
					endif
				endif
			endif
		endif
		;Fall Through...
;***********************************************
	.sbttl "Award Bonus Points"
;***********************************************
award		lda	bonusa
		ifne				;Skip if 0
			lda	shipst		;Skip if Dying or Dead
			ifne
				ifpl	
					lda	mzgame
					and	#$1C			;Only during these times
					ifne
						lda	bonsnd		;Bonus Sound Started??
						ifeq
							lda	#$snd_c7		;Do Bonus Sound
							sta	bonsnd
							jsr	dosound
						endif
						lda	frame
						and	#03			;Add in slowly
						ifeq
							sed				;********** Caution **********
							lda	bonusa
							sec
							sbc	#01
							sta	bonusa		;Add another 100
							cld				;*****************************
							ifeq				;Down to 0??
								sta	bonsnd
								lda	#snd_c6		;Stop Bonus Sound
								jsr	dosound
							endif
							lda	#01
							ldx	#00
							jsr	bpont3
						endif
					endif
					lda	mzgame		;Skip message only in tact scan
					and	#$20			;In Tact??
					ifeq				;not tact
						ldx	#mbonus
						jsr	mesg			;Bonus message
						lda	#00
						sta	temp1			;LSB fake 0
						lda	bonusa
						sta	temp1+1
						ldy	#02
						lda	#temp1
						sec
						jsr	digits
					endif
				endif
			endif
		endif
		rts	

;******************************************************
	.sbttl "Start Space Play Wave from Transition"
;******************************************************
dospace	lda	#04			;Set to Third person play level
		sta	mzgame		;Must be at 3rd play point
		sta	spcspd		;Stars on take off speed
		lda	#$3F
		sta	mtim			;Display Message if needed
		lda	#-7		
		sec	
		sbc	dif4mz		;-7 to -12
		sta	statst		;Turn on space station and start stars
		lda	#$73
		sta	stsclh
		lda	dif4mz
		clc	
		adc	incdif
		adc	#04
		cmp	#08
		ifcs
			lda	#08
		endif			;Get Speed of Landing
		lsr	A
		ldy	rands
		ifmi
			jsr	neg
		endif
		sta	shpvel+1
		lda	#00
		sta	statxl
		sta	stscll
		sta	gunctl
		sta	gunctl+1		;Close the guns
		lda	#$0C
		sta	statyh		;Initial Position of Space Ship
		lda	#$80
		sta	statyl
		sta	statxl		;LSB's at 80
		lda	#center
		sta	statxh		;Center of Screen X
		rts	
		
;***********************************************
	.sbttl "Blow Base Ship Sequence Control" 
;***********************************************
;* Used to control action of base star and     *
;* player ship for the blow up base star       *
;* sequence                                    *
;***********************************************
blowit	lda	frame
		and	#07
		tay				;Save for anybody who needs this below
		lda	#$20			;Set NE test for below
		bit	blowst		;Already Blown??
		ifvs				;Keep Moving Backward
			jsr	pictran		;Change to end pic
			lda	spcspd		;Get Star speed to 5
			cmp	#06
			ifcs				;Star Speed at 5
				tya
				ifeq
					dec	spcspd
				endif
			endif
			lda	stroyh		;Would like stars near center
			ifne
				lda	stroyl
				sec
				sbc	#$20			;Move down star origin
				sta	stroyl
				ifcc
					dec	stroyh
				endif
			endif
		else
			ifne				;We wish to move forward...
				lda	spcspd		;Start decel move
				cmp	#09
				ifcc
					tya
					ifeq
						inc	spcspd		;Slow down first
					endif
					
				else
					lda	#00
					sta	blowst		;Set flag for accel again
					sta	rearview		;Set to forward again
				endif
			else				;Want to move forward now
				lda	spcspd
				cmp	#03
				ifcs				;Speed up stars again
					tya
					ifeq
						dec	spcspd
					endif
				endif
			endif
		endif
		;**************** End Star Control ***************
		lda	blowst
		ifpl				;Skip if we already started this
			and	#$10			;Moving Base Ship??
			ifne				;yes
				lda	statyl		;Move up from bottom
				sec
				sbc	#$40
				sta	statyl
				lda	statyh
				sbc	#00
				sta	statyh		;Move up ship
				lda	stscll
				clc
				adc	#$0C			;Move away speed
				ifmi
					sec
					sbc	#$80
					inc	stsclh
				endif
				sta	stscll
				lda	stsclh
				cmp	#$75
				ifeq
					lda	#$C8
					sta	blowst		;BOOM!!!
					sta	frame			;Force Frame for Flash effect
					jsr	dostop
					lda	#snd_stm		;Stop Music
					jsr	dosnd2		;Always!
					lda	#$76
					sta	blsclh
					sta	blscll		;Start Scale of pieces
					lda	shipst		;Must save this
					pha
					jsr	blowship		;And use his peices to do explosion
					pla
					sta	shipst		;Restore (clobbered by blowsh)
				endif
			endif
		else
			;**************  End Station Control **************
			;************ Now add explosions if needed ********
			ldx	#$nmexp-1		;A few more peices
			jsr	piece2		;Do Explosion
			lda	blsclh		;Big Already??
			cmp	#$71			;Max Size
			ifcs				;Not there yet
				lda	blscll
				sec
				sbc	#05
				ifmi
					sec
					sbc	#$80			;Keep in bottom 80
					dec	blsclh
				endif
				sta	blscll		;Save back LSB
				
			else				;Are there
				lda	#$20
				sta	blowst		;Next Wave
			endif
			lda	frame
			cmp	#$D8
			ifcs				;Still white explosion pic
				ldy	#$C0
				sty	blowst		;Drop white bit
			endif
			and	#03			;Time for another explosion?
			ifeq				;yes
				ldx	nxtexp		;Pointer to next guy
				cpx	#nmform		;Any left??
				ifne
					ldy	#00
					jsr	getrand
					and	#$3F
					bit	rands			;Bit Random Number
					ifmi
						jsr	neg			;Negative Number
						dey
					endif
					clc
					adc	statyl
					sta	sobjyl,X
					tya
					adc	statyh
					clc
					adc	#05			;Compensate for base being not offset by cr3p
					sta	sobjyh,X
					lda	#$80
					clc
					adc	sobjyl,X
					sta	sobjyl,X
					ifcs
						inc	sobjyh,X		;Rest of Compensation
					endif
					ldy	#00
					jsr	getrand
					and	#$3F
					bit	rands+1
					ifmi
						jsr	neg			;Negative Number
						dey
					endif
					clc
					adc	statxl
					sta	sobjxl,X
					tya
					adc	statxh
					sta	sobjxh,X
					lda	#$80
					sta	sobjst,X		;Explode it!
					lda	#snd_j3
					jsr	dosound
					inc	nxtexp
				endif
			endif
		endif
moveexp	;Entry from below in inblow
		ldx	#nmform-1
		begin
			stx	temp9
			lda	sobjst,X		;Exploding this one?
			ifmi
				sta	temp3			;drawen wants this
				clc
				adc	#04
				ifpl
					lda	#00
				endif
				sta	sobjst,X
				jsr	copypos		;Copy position to xcomp
				jsr	drawen2		;Draw this guy
			endif
			ldx	temp9
			dex
		miend
		rts	
		
inblow	ldx	nxtexp
		cpx	#nmform
		ifne
			lda 	frame
			and	#01
			ifeq
				lda	inblotx,X
				sta	sobjxh,X
				lda	inbloty,X
				sta	sobjyh,X
				lda 	#$80
				sta	sobjyl,X
				sta   sobjxl,X
				sta	sobjst,X
				lda	#$45
				jsr	dosound
				inc	nxtexp
			endif
		endif
		jmp	moveexp
					
inblotx	.byte $04,$04,$04,$05,$05,$05,$03,$03,$03,$02,$08,$04,$04,$05,$06,$04
inbloty	.byte $06,$07,$08,$07,$08,$06,$08,$06,$07,$08,$07,$07,$09,$05,$04,$05

;********************************************
	.sbttl "Base Ship Control"
;********************************************
;* Start Space Game (Move ship upscreen and *
;* away). If skip flag (lauen=40), then     *
;* just go to passby wave.                  *
;********************************************
startspace	ldx	maznum		;Get Start Positions
		bit	lauen			;Skip Space Fight??
		ifvs				;yes... use X=4
			lda	#$10			;Value to store to lauen
			ldx	#00			;Will use item 0 for skip mode
		else				;Do this if not skip
			lda	#09		
			sta	shipyh
			lda	#$C0
			sta	shipyl
			lda	#00
		endif
		sta	mzgame		;Store Game Play
		lda	#$80
		sta	statst		;Also turn on Station
		lda	s_statm,X		;Station Motion
		sta	stbflg		;Motion Up Flag
		lda	s_statx,X		;Station XH
		sta	statxh
		lda	s_statxl,X
		sta	statxl
		lda	s_staty,X
		sta	statyl		;Set Y
		sta	statyh		;*** Temp Start Position ***
		lda	#00
		sta	saucvl		;Stop Saucer Motion
		sta	saucvd
		rts	
		
s_statm	.byte $80,$C0,$00,$80		;Station Motion
s_statx	.byte center,0,center,center	;Position MSB
s_statxl	.byte $80,$10,$80,$80		;Position LSB
s_staty	.byte $00,$07,$00,$00		;Y MSB Position

;********************************************
	.sbttl "Approach Target"
;********************************************
;* This routine is used to allow the ship to*
;* approach a destination position for auto *
;* ship control.                            *
;*                                          *
;* Input: X = Select of Target Destination  *
;********************************************
targship	ldy	#00			;Signal not there yet
		sty	temp1+1		;Clear Flag for zooming out, LSB force mode
		lda	shipxh		;Do X First
		pha	
		lda	mzgame		;Special Target for Zoom
		cmp	#02			;Zooming?
		bne	?at5			;No, Do normal.
		bit	gamest		;Zooming Out??
		ifvs				;yep!
			dec	temp1+1		;Set flag
			sec
			lda	shipxl
			sbc	landsl		;Back to landing spot
			sta	temp1
			pla
			sbc	landsh		;Move Back to original Landing Sight
		else
?at5			lda	shipxl
			sec
			sbc	targxl,X
			sta	temp1			;Save for LSB check
			pla
			sbc	targx,X		;X Target
		endif
		ifne				;Not there yet
?at6			ifvs
				eor	#$80
			endif
			ifpl				;To Right of Target
				dey
				lda	mvspdx,X
				jsr	neg2
			else
				lda	mvspdx,X
			endif
			sta	shpvel+1
			clc
			adc	shipxl
			sta	shipxl
			tya
			adc	shipxh
			sta	shipxh
			iny				;Insure positive number here
		else
			;Could approach from right side of target, MSB would be 0
			lda	temp1			;Any LSB's
			and	#$F8			;Drop 0-7
			ifne				;Not at target
				lda	#00			;Make MSB 0
				beq	?at6
			endif
			bit	temp1+1		;Zooming Out??
			ifmi				;yep
				lda	shipxl		;Just force back, skip approach
				sta	landsl		;Force LSB's to be the same
			endif
			ldy	#$80			;At target X
		endif			;End of X
		sty	targf			;Stor X at Target Part
		;--------------------------------------------------------------
		;------------- Now Check Y Part -------------------------------
		;--------------------------------------------------------------
		ldy	#00			;Signal Not at Target
		lda	shipyl
		cmp	targyl,X		;LSB change too
		lda	shipyh
		sbc	targy,X		;At Y Target
		ifne				;Not there
			ifvs
				eor	#$80
			endif
			ifpl				;To Right of Target
				dey				;Sign prop
				lda	mvspdy,X		;Move Speed
				jsr	neg2
			else				;Move in from Right
				lda	mvspdy,X
			endif
			clc	
			adc	shipyl
			sta	shipyl
			tya	
			adc	shipyh
			sta	shipyh
			iny				;Insure + Number
		else
			ldy	#$80
		endif			;End of Y
		tya	
		and	targf
		sta	targf			;Both must be 80 for done
		;----------------------------------------------------------------
		ldy	#00			;Guess scale not at a target
		lda	shpsch		;Want to shrink down ship
		cmp	tarsch,X		;Down to target??
		ifne
			ifcs
?at10				lda	shpscl		;Change Scale Down
				sec
				sbc	mvscal,X		;Change Scale
				ifmi				;Went past 0
					sbc	#$80			;Stay within lin < 80
					dec	shpsch		;Linear down by 1
				endif
			else
?at20				lda	shpscl
				clc
				adc	mvscal,X		;Change up
				ifmi				;Went past 80
					sec
					sbc	#$80
					inc	shpsch		;Bin up by 1
				endif
			endif
?at25			sta	shpscl
		else				;Bin is the same
			sec
			lda	shpscl
			sbc	tarscl,X		;Check Linear
			ifne
				ifcs					;Target is smaller
					cmp	mvscal,X			;Smaller diff than change??
					ifcc					;yes
?at28						lda	tarscl,X			;Then just set at target
						;************** Caution, never set a target at 0 ************
						jmp	?at25				;Go and Store
					endif
					jmp	?at10				;Else do normal
				endif
				;Not CS... Must be CC, that is target is greater!
				jsr	neg				;Get + Difference
				cmp	mvscal,X			;Diff less than Scale??
				bcc	?at28				;Just store it then
				bcs	?at20				;Else do normal
			endif
			ldy	#$80				;Must be at target
		endif
		tya	
		and	targf
		sta	targf			;Put in with Targets!
		;******************* Now Check for Base Scale Change if needed *********************
		lda	mzgame
		and	#02
		ifne				;Game 2 needs it!
			bit	gamest		;Want to grow??
			ifvc				;yes
				lda	stsclh		;Grow to target size
				cmp	#sttarh		;Up to target
				ifne				;Not there yet
?at30					lda	stscll		;Change scale down
					sec
					sbc	mvscal,X		;Change Scale
					ifmi				;Went past 0
						sbc	#$80			;Stay with lin < 80
						dec	stsclh		;Linear down by 1
					endif
					sta	stscll
				else				;High byte at target, check low
					lda	stscll
					cmp	#$10			;... check linear
					bcs	?at30
					;********************* Zoom in Achieved ****************************
					jsr	bumpgame		;Start next game
				endif
			else				;Flag says shrink down again
				lda	stsclh		;Shrink down to target size
				cmp	#sttarh+2		;At size??
				ifne				;Not there yet
?at40					lda	stscll
					clc
					adc	mvscal,X		;Change up
					ifmi				;Went past 80
						sbc	#$80
						inc	stsclh		;Bin up by one
					endif
					sta	stscll
				else				;Bin is the same
					lda	stscll
					cmp	#$10
					bcs	?at40			;Add till wraps 0 to 5
					lda	#07
					sta	mzgame		;We are out, take off
					lda	#$8F			;Start stars and station moving
					sta	statst
					lda	#01
					sta	manstat		;In case off in attract
					lda	#snd_a2b2
					jmp	dosound
				endif
			endif
		endif
		rts
			
;*******************************************
	.sbttl "Pick Picture"
;*******************************************
picpic	ldx	#plane7			;Stat, Page Select (Might be needed)
		lda	shipst
		ifeq
?pp1			jmp	pic4
		endif
		ifmi					;Blowing Up??
			ldx	#plane7+sparkle		;Stat, Page Select, Sparkle
			bne	?pp1				;And skip the rest
		endif
		lda	tact
		bne	?pp2				;3rd person for Tact Display
		lda	mzgame
		tay					;Save Copy
		and	#$C4				;In 3rd Person Play??
		;Above also is an escape for tunnel pic as nothing is needed for that
		ifne
?pp2			lda	#third			;Always third person here
			sta	shppic
			bne	?pp1				;Skip it!
		else
			lda	shipxh
			cmp	#center
			ifcc					;It's on right
				txa
				ora	#04				;Flip Bit (60 to 61 or 68 to 69)
				tax
			endif
			tya					;Recall mzgame
			and	#$0F				;All these don't do this routine!!
			bne	?pp1
		endif
pic2		ldy	#00				;Guess Flat
		lda	shpvel+1			;Ship Velocity X
		ifmi
			bit	lastflip			;Already flipped
			ifmi					;If yes, hold til down to -5
				cmp	#-3
				ifcc
pic3					ldy	#picpos2-picpos	;Move to tilt left
				endif
			else					;Else, don't flip till at 10
				cmp	#-10d				
				bcc	pic3				;Okay to flip
			endif
		else
			bit	lastflip			;Already flipped
			ifmi					;If yes, hold till under 5
				cmp	#03
				ifcs
?pp10					ldy	#picpos3-picpos		;Move to tilt right sequence
				endif
			else					;Else, wait till at 15
				cmp	#10d
				bcs	?pp10				;Time to flip
			endif
		endif
		tya					;Pic Base
		sta	lastflip			;If 0, we just cleared this flag
		ifne					;We flipped
			ldx	#$80
			stx	lastflip			;so set flag
		endif
		lda	shipxl
		sta	temp2
		lda	shipxh
		sta	temp2+1			;T
		asl	temp2
		rol	temp2+1			;2T
		clc	
		lda	shipxl
		adc	temp2
		sta	temp2
		lda	shipxh
		adc	temp2+1			;2T+T
		sta	temp2+1
		sec	
		lda	temp2
		sbc	#$80
		sta	temp2
		lda	temp2+1
		sbc	#07				;-((rtedge-4)*3/2)
		sta	temp2+1			;(2T+T)-180
		sta	shppbs			;Save for fire shot routine
		tya	
		clc					;Add in position select
		adc	temp2+1			;Add position select to tilt info
		tay	
		lda	picpos,Y			;Get proper picture from position
		sta	shppic
		ldx	pictil,Y			;Get tilt info
pic4		lda	#$E7				;Temp color
		ldy	shipst			;Blowing Up??
		beq	?pp20				;Or dead??
		ifmi
?pp20			tya
			eor	#$70				;Dim out as blowing up
		endif
		sta	temp8				;Save color info here
		stx	temp8+1			;Save flip info here
		jsr	vgadd2			;Add Stat Instruction
		rts	
		
picpos		.byte $01,$02,$03,$04,$05,$06,$06,$05,$04,$03,$02,$01

picpos2		.byte $0D,$0E,$0F,$10,$11,$0C,$0C,$0B,$0A,$09,$08,$07		;Tilt Left
picpos3		.byte $07,$08,$09,$0A,$0B,$0C,$0C,$11,$10,$0F,$0E,$0D		;Tilt Right

pictil		.byte plane7,plane7,plane7,plane7,plane7,plane7
			.byte plane7+xflip,plane7+xflip,plane7+xflip
			.byte plane7+xflip,plane7+xflip,plane7+xflip
			.byte plane7,plane7,plane7,plane7,plane7,plane7+xflip
			.byte plane7+xflip,plane7+xflip,plane7+xflip
			.byte plane7+xflip,plane7+xflip,plane7+xflip
			.byte plane7,plane7,plane7,plane7,plane7,plane7
			.byte plane7,plane7+xflip,plane7+xflip,plane7+xflip
			.byte plane7+xflip,plane7+xflip

;*****************************************
;* Used by 'trans' routine to do upward  *
;* /downward transition on pictures from *
;* 1st to 3rd person and back.           *
;*****************************************
pictran	lda	frame
		and	#07			;Change Time
		ifeq
			lda	shppic		;Will need to check for ends
			bit	gamest		;Up or Down??
			ifvc
				cmp	#third		;Made it to third yet??
				ifne
					inc	shppic
				endif
			else
				cmp	#stthird		;Make it back to first yet??
				ifne
					dec	shppic
				endif
			endif
		endif
		rts	
		
;**************************************
	.sbttl "Ship Piece Explosions"
;**************************************
;* If ship is exploding, used to move *
;* and output ship pieces.            *
;**************************************
pieces	bit	shipst			;Blowing up??
		ifpl					;Nope
			rts
		endif
		ldx	#(nmexp/2)-1		;Explosion Pieces
piece2	;Entry for Base Ship Explosion
		begin
			stx	temp9				;Save a copy
			lda	sxst,X			;See if this peice active
			ifeq
				jmp	?spe10
			endif
			lda	frame
			and	rspeed,X			;How fast to roll
			ifeq
				inc	sxst,X			;Next Pic
				lda	sxst,X
				and	#$17
				sta	sxst,X			;One of 8 pics and active bits
			endif
			ldy	#00
			lda	sxyv,X
			ifmi
				dey
			endif
			clc
			adc	sxyl,X
			sta	sxyl,X
			sta	xcomp+2
			tya	
			adc	sxyh,X
			sta	sxyh,X
			sta	xcomp+3
			ldy	#00
			lda	sxxv,X
			ifmi
				dey
			endif
			clc	
			adc	sxxl,X
			sta	sxxl,X
			sta	xcomp
			tya	
			adc	sxxh,X
			sta	sxxh,X
			sta	xcomp+1
			;----------------------------------------------
			;---------- Now Draw this peice ---------------
			;----------------------------------------------
			jsr	cor3p				;Position and correct for 3rd person
			lda	shpscl
			ldx	shpsch			;Current Ship Scale
			bit	blowst			;Station Blowing??
			ifmi					;Yep, Different scale
				lda	blscll
				ldx	blsclh
			endif
			inx	
			jsr	vgadd2			;Proper Scale Added
			lda	shipst			;We know this to be 80 or off
			eor	#$FF
			and	#$78
			asl	a
			ora	#07				;White
			bit	blowst			;Station (Again!!)??
			ifmi					;Yep!
				eor	#05				;Make it Green
				bit	rands+2			;Bit a Random
				ifpl
					ifvs					;Random !?!?!?!?!
						lda	#$FB				;Bright Red!
					endif
				endif
			endif
			ldx	#sexps7			;Stat Instruction
			jsr	vgadd2			;Add Color
			;-------------- Which Piece is This? ---------------------
			ldx	temp9				;Recall X
			lda	sxst,X
			and	#07
			asl	a
			clc	
			bit	blowst			;Doing Base Explosion??
			ifmi
				adc	basepic,X
				tay	
				lda	bxp0s,Y
				ldx	bxp0s+1,Y
			else
				adc	wpiece,X			;Which Piece?
				tay					;Now points to the jsrl
				lda	sxp0s,Y
				ldx	sxp0s+1,Y			;Have jsrl
			endif
			jsr	vgadd2			;Add Pic
?spe10		ldx	temp9				;Recall X
			dex
		miend				;We are Done
		rts
		
;Rotation Speed of Each Piece	
rspeed	.byte $03,$01,$07,$03,$01,$07,$03,$01,$00,$01,$07,$03

;Which Picture
wpiece	.byte sxp0s-sxp0s		;Piece 0 jsrl
		.byte sxp1s-sxp0s   
		.byte sxp2s-sxp0s
		.byte sxp3s-sxp0s
		.byte sxp1s-sxp0s
		.byte sxp3s-sxp0s
basepic	.byte bxp0s-bxp0s
		.byte bxp1s-bxp0s
		.byte bxp1s-bxp0s
		.byte bxp0s-bxp0s
		.byte bxp1s-bxp0s
		.byte bxp0s-bxp0s
		.byte bxp0s-bxp0s
		.byte bxp1s-bxp0s
		.byte bxp0s-bxp0s
		.byte bxp0s-bxp0s
		.byte bxp1s-bxp0s
		.byte bxp1s-bxp0s
		

;************************************************
	.sbttl "Move Ship"
;************************************************
;* This Routine is for moving the ship from the *
;* rolly-gig. It also controls any special Y    *
;* Motion that may be needed.                   *
;************************************************		
moveship	bit	shipst			;Skip if Exploding
		bpl	?ms50
?ms49		jmp	?ms10
?ms50		bit	gamest
		bpl	?ms51
		ifvs
?ms51			jmp	?ms1				;Take Off, Skip this
		endif
		ldy	#00
		lda	mzgame
		and	#$FA				;So far games 4 and 0
		bne	?ms49
		lda	mzgame
		cmp	#04				;Landing??
		ifeq					;Might be
			ldy	#00
			lda	rgdd				;Any motion??
			ifne					;Skip this if not moving
				ifmi
					dey
				endif
				clc	
				adc	shpvel
				sta	shpvel
				tya	
				adc	shpvel+1
				sta	shpvel+1
			endif
			ldy	#00
			tax					;Save new velocity
			lda	frame
			and	#$1F
			ifeq
				txa
				ifne
					ifmi
						inc	shpvel+1
					else
						dec	shpvel+1
					endif
				endif
			endif
			lda	shpvel+1			;Recall for below position add
		else
			lda	rgdd				;Rolly Gig data
			sta	shpvel+1			;For Tilt Info
		endif
		ifmi
			dey				;Propagate Sign	
			cmp	#-$40
			ifcc
				lda	#-$40			;Limit - Velocity
			endif
		else
			cmp	#$40
			ifcs
				lda	#$40			;Limit + Velocity
			endif
		endif
		sta	shpvel+1			;Restore velocity (in case changed)
		bit	lasst				;Laser Active??
		bmi	?ms2				;yes, so don't move in X
		clc	
		adc	shipxl			;Add to position
		sta	temp2				;Save LSB
		tax					;Save LSB for later
		tya	
		adc	shipxh
		tay					;Save MSB for real store later
		sta	temp2+1			;Hold LSB
		;------------------ Now Do Limit Check --------------------
		sec	
		lda	temp2				;(rtedge/2)-2
		sbc	#$80
		lda	temp2+1
		sbc	#02
		ifmi
?ms2			lda	#00
			sta	shpvel			;Stop Velocity on edges
			sta	shpvel+1
			beq	?ms1				;And Skip this mess
		endif
		cmp	#04				;Range Check
		bcs	?ms2				;Don't go to 4 (3.FF is max)
		tya					;Recall 'real' Position
		sta	shipxh
		txa					;Recall LSB
		sta	shipxl
?ms1		lda	mzgame
		and	#04
		ifne					;Game 4, move down slowly
			lda	dif4mz			;Get Landing Speed
			clc	
			adc	#05				;5 Min
			sta	temp3				;Save this
			lda	shipyl
			bit	gamest			;Take off or land??
			ifvc					;Land
				ldx	#00
				stx	nosync			;Clear Fast Flag too
				clc
				adc	temp3				;Add in velocity
				tax
				lda	shipyh
				adc	#00
				ifpl
?ms5					stx	shipyl
					sta	shipyh			;Move Y a little
				endif
			else					;Taking Off!
				sec	
				sbc	#$10
				tax	
				lda	shipyh
				sbc	#00
				cmp	#03			;At top??
				bne	?ms5			;Okay to store
				lda	#01
				sta	mzgame		;Blow Ship Time
				lda	#08
				sta	spcspd		;Start with slow stars
				lda	#$40
				sta	blowst
				lda	#$80
				sta	rearview		;Start Stars moving backwards
				lda	#$0C
				sta	statyl		;Position of Station
				sta	statyh		;Init Position of Station
				lda	#05
				sta	stroyh
				sta	stroyl
				lda	#$70
				sta	stsclh		;Initial Station Scale
				ldx	#maxstr-1
				lda	#00
				sta	statst		;Station off for now
				sta	stbflg		;And no motion
				sta	stscll		;Initial Scale of Station
				sta	nxtexp		;Init exp Index
				begin
					sta	strflg,X
					sta	strxh,X
					sta	stryh,X		;Clear old stars
					dex
				miend
			endif
		endif
?ms10		jsr	rgdr				;Scroll down rgdd data
		;******************* Fall Through *********************************
		
;*********************************************
	.sbttl "Output Ship to Screen"
;*********************************************
;* Add ship picture at proper size to vglist *
;*********************************************
shipdis	lda	blowst
		and	#$10				;Station if in front of us
		bne	?oss1				;Just leave then
		lda	shipst			;Ship active??
		ifeq
?oss1			rts					;Nope!
		endif
		;************** Place Ship Now! ********************8
		ldx	#03
?oss2		lda	shipxl,X
		sta	xcomp,X
		dex	
		bpl	?oss2
		lda	mzgame
		ifne
			jsr	cor3p
		else
			lda	#00
			sta	scalef
			sta	scalef+1
			jsr	posvec
			ifmi
				rts					;Not Drawn
			endif
		endif
		lda	shipst			;Blowing up?
		ifmi
			cmp	#$D0				;Do First part Fast
			ifcc
				lda	shipst
				adc	#03
				sta	shipst
			else
				lda	frame
				and	#01
				ifeq
					inc	shipst
					inc	shipst		;Slow down at end
				endif
			endif
			lda	shipst			;Set Sign just in case
			ifpl					;It's Done!
				lda	#00
				sta	manstat			;For now, you die!
				sta	shipst
			endif
		endif
shipdis2	ldx	shpsch
		lda	shpscl			;Get Ship's Scale
		inx	
		jsr	vgadd2			;Scale for ship
		lda	#00
		sta	xcomp
		sta	xcomp+1
		sta	xcomp+3			;Correction vector for bottom of ship
		lda	#$70
		sta	xcomp+2
		jsr	vgvtr2
		jsr	picpic			;Pick correct picture
		bit	mzgame
		ifvs					;In tube??
			ldy	#$30				;Tube pic only
		else
			lda	shppic
			asl	a
			tay
		endif	
		lda	shipst			;Blowing up??
		ifmi
			cmp	#$D0				;Only 5 pics
			ifcc
				and	#$F0
				lsr	A
				lsr	A
				lsr	A
				sec	
				sbc	#$10
				pha	
				jsr	addfpic			;Left Half
				lda	temp8+1
				eor	#04
				tax	
				lda	temp8
				jsr	vgadd2
				pla	
				jsr	addfpic
			endif
		else
			lda	planes-2,Y			;-2 -- No Plane 0
			ldx	planes+1-2,Y		;From Source
			jsr	vgadd2			;Add ship picture
		endif
		lda	shpscl
		ldx	shpsch
		cpx	#$71				;Too Large
		ifcc					;yes
			rts					;Just Skip it!
		endif
		jsr	vgadd2
		lda	tact				;Tact display on??
		ifne					;yes
			jsr	tactdi
		endif
		lda	#00				;Just restore normal STAT
		ldx	#$60
		jsr	vgadd2
		jmp	pieces			;Add pieces if necessary
		
addfpic	tay					;Add Fighter Pic
		lda	sexps,Y
		ldx	sexps+1,Y
		jmp	vgadd2
		
		
;***********************************************
	.sbttl "Blow Up Ship (Start Sequence)"
;***********************************************
blowship	txa	
		pha				;Save X Reg
		jsr	dodelay
		lda	#snd_c1
		jsr	dosound
		lda	#$80
		sta	shipst		;Start Explosion
		ldx	#nmexp-1
		begin
			jsr	getrand		;Random Start Spin
			and	#07
			ora	#$10
			sta	sxst,X		;Turn on a piece
			lda	shipxl
			sta	sxxl,X
			lda	shipxh
			sta	sxxh,X
			lda	shipyl
			sta	sxyl,X
			lda	shipyh
			sta	sxyh,X		;Position Pieces
			jsr	getrand
			and	#07
			jsr	signc			;Get Sign
			sta	sxxv,X	
			jsr	getrand
			and	#$1F
			bit	blowst
			ifpl
				and	#07
			endif
			jsr	signc			;Get Sign
			sta	sxyv,X		;Start it moving
			dex
		miend
		pla	
		tax				;Restore X
		rts	
		
signc		pha				;Save this A
		jsr	getrand		;Random Direction
		ifmi
			pla
			jmp	neg
			pha			;***************** Remove this later *************** JA
		endif
		pla
		rts
			
;***********************************************
	.sbttl "Shoot Out Tube"
;***********************************************
;* The routine will sequence the tube to allow *
;* the player to shoot down it! Uses the table *
;* of JSRL's in TWROM module & tcount as which *
;* of 32 sequences to use.                     *
;***********************************************
tube		bit	tstat
		ifpl				;Launch on hold
			bit	mzgame
			ifvs				;Only check this if in lauch mode
				;Want the man to stand for a bit, then run to ship
				lda	mazvxl
				bne	?tu10			;This is already done
				lda	frame
				and	#$3F
				ifeq
					lda	#$20
					sta	mazvxl
					lda	#05
					sta	mazvyl
				endif
			endif
			jmp	?tu10			;Always
		endif
		bit	tcount
		ifpl
			dec	tcount
			ifmi				;Done here
				lda	tactde		;Will tell us if....
				sta	tact			;Must do tact display
				ifmi
					lda	maznum
					sec
					sbc	#01
					and	#03
					sta	seqp			;Init Pic
				endif
				lda	#00
				sta	seqst			;Clear Status
				sta	seqx			;Start Over
				lda	#$20			;Out of tube, into transition
				sta	mzgame
				lda	#$3F
				sta	mtim
				lda	#snd_a2b2
				jmp	dosound
			endif
?tu10			lda	tcount
			cmp	#$20			;In Extension??
			ifcs
				and	#03
				adc	#$1A			;Add 1B (carry is set)
			endif
			tay				;Set Sign bit
			ifpl				;- is inactive
				pha				;Save this
				lda	#00
				ldx	#$72
				jsr	vgadd2		;Set Scale
				lda	#$F1
				ldx	#tube7		;Stat, page select
				jsr	vgadd2		;Add Color
				pla				;Sequence #
				eor	#$FF
				clc
				adc	#32d			;Subtract from 32
				asl	a			;For Words
				tay
				lda	movet,Y
				ldx	movet+1,Y
				jsr	vgadd2
			endif
		endif
		bit	tstat			;Skip if moving
		ifpl
			ldx	player
			lda	lives,X		;Number of lives
			sec
			sbc	#01			;Count the one we have
			ifne
				ifpl
					cmp	#05
					ifcs
						lda	#04			;Show Only 4 Please
					endif
					ifne
						sta	temp8
						lda	#-4
						sta	xcomp+1
						lda	#$15
						sta	xcomp			;Initial Position
						lda	#-3
						sta	xcomp+3
						lda	#$49
						sta	xcomp+2
						lda	#00
						sta	vgbrit
						lda	#mancol+$E0		;Set up Color for all these
						ldx	player		;Player 2??
						ifne				;Yes, change color
							lda	#mancol2+$A0		
						endif
						ldx	#mpic7		;Stat, page select
						jsr	vgadd2
						lda	#$98			;Add Scale Instruction
						ldx	#$72
						jsr	vgadd2		;Man's Scale
						begin			;Output Lives
							jsr	vgcntr
							jsr	vgvtr2		;Position for this one
							lda	#$1B			;Arms crossed pic
							jsr	picout		;Do this one
							lda	xcomp
							sec
							sbc	#$58
							sta	xcomp
							ifcc
								dec	xcomp+1
							endif			;Position for possible next one
							lda	xcomp+2
							sec
							sbc	#$0D
							sta	xcomp+2
							ifcc
								dec	xcomp+3
							endif
							dec	temp8
						eqend
					endif
				endif
			endif
		endif
		rts
		
;***********************************************
	.sbttl "Space Station Picture and Motion"
;***********************************************
;* The routine moves and places the space      *
;* station. One of 4 space station pics are    *
;* used.                                       *
;*                                             *
;* Inputs: stat[xl,xh,yl,yh,st]                *
;*         mzgame                              *
;***********************************************
statot	lda	statst			;Station Active??
		ifpl					;nope
?ssp1			rts					;Just skip this then
		endif
		lda	mzgame
		cmp	#08				;Skip on transition
		beq	?ssp1
		and	#$EF				;Other case is 3rd person views
		ifeq					;Third person??
			jmp	statfb			;Do first person views
		endif
		;Is Third Person View, Set Landing Sight Pic
		lda	statst			;Recall Station Status
		ldy	#00				;Sign Extend
		and	#$7F				;Drop Active, pick up speed
		cmp	#$40				;7 bit signed number
		ifcs
			ora	#$80				;Prop Sign
			dey
		endif
		clc	
		adc	statyl
		sta	statyl
		sta	xcomp+2			;For Output
		tya	
		adc	statyh
		sta	statyh			;New Position
		sta	xcomp+3			;For Output
		cmp	#$0C
		ifcs
			lda	blowst			;Blowing Up?
			ifeq					;Nope
				tya					;Moving Down??
				ifpl					;yep
					lda	#$80
					sta	statst			;Station Status to top
				endif
				jmp	?ssp1
			endif
		endif
		tay					;Save MSB
		;----------------- Set Landing Sight Position ----------------------------
		lda	statxl
		sta	landsl
		lda	statxh
		sta	landsh
		;----------------- Sight Uses Same Y as Base Ship ------------------------
		lda	statxl
		sta	xcomp				;Save for output
		sec	
		sbc	shipxl			;Get ship relative to station
		sta	temp1				;Save in temp for possible negate
		lda	statxh
		sta	xcomp+1			;For station output later
		sbc	shipxh
		sta	temp1+1
		ifmi					;Want absolute value
			jsr	dblneg			;Negative temp1
		endif
		;Returns A = temp1+1 = MSB of difference
		ldx	mzgame			;Skip this if not needed
		cpx	#04
		ifeq
			bit	shipst			;We can skip this if blowing up
			ifpl
				ldx	maznum			;Which Maze??
				cmp	mazxsh,X			;Chech MSB of maze site
				bcc	?ssp10			;Okay, we are above the maze....
				ifeq					;If it's the same here
					lda	temp1				;...else check LSB amount over
					cmp	mazxsl,X			;Additional LSB size
					bcs	?ssp30			;Not above here either
		; We are above the maze here... Now check Y level for landing
?ssp10				jsr	ychk
		; Above returns A= MSB difference, temp1 = LSB difference
		; Also, temmp1+1 = MSB
					ifmi					;Landing Level achieved?
					;Now must recheck X position to see if we landing good or bad
						jsr	landit
					endif
				else					;Not above base ship
?ssp30				jsr	ychk				;Do we need to shoot him?
					ifmi					;He is past!, Shoot the sucker.
						lda	shipst			;Open only if ship active
						ifne
							ifpl
								lda	#02
								ldx	#00			;Guess Gun 0
								ldy	shipxh		;Which Side??
								dey
								dey	
								dey				;Cheap way to see which side of 3	
								ifpl	
									inx
								endif
								ldy	gunctl,X			;Already opening?
								ifeq					;no, okay to open
									sta	gunctl,X			;Open this gun
									lda	#snd_h3
									jsr	dosound
								endif
							endif
						endif
					endif
				endif
			endif
		endif
		lda	mzgame			;Blow up sequence?
		cmp	#01
		ifeq
			lda	blowst			;Blowing Up?
			and	#$18
			ifne
				lda	#00
				sec	
				sbc	xcomp+2
				sta	xcomp+2
				lda	#00
				sbc	xcomp+3
				sta	xcomp+3			;Negate, Y runs wrong direction
				jsr	cor33				;Complete Correction
				jsr	cor				;Place (Output)
				lda	stscll			;Set new size
				ldx	stsclh
				inx	
				jsr	vgadd2
				jmp	fpbase			;And draw first person
			endif
			rts
		endif
		lda	#$F2
		ldx	#st7				;Stat, page select
		jsr	vgadd2			;Add color
		lda	stscll
		sec	
		sbc	#$40
		lda	stsclh
		sbc	#$71				;Is it under71 40??
		ifmi					;yep
			lda	#-1
			sta	xcomp+1			;Position Line!
			jsr	cor3p
			lda	#00
			ldx	#$71
			jsr	vgadd2			;Always scale 0
			laljsr(longline)
			lxhjsr(longline)			;Just draw a long line
		else
			jsr	cor3p				;Correct for 3rd person and draw
			lda	stscll
			ldx	stsclh
			inx	
			jsr	vgadd2			;Scale of base
			lda	maznum			;Which ship to draw
			asl	a
			tay	
			lda	bases,Y
			ldx	bases+1,Y
		endif
		jsr	vgadd2			;Draw pic and return
		;******************* Now Draw Landing Sight *************************8
		ldy	#00
		lda	mzgame			;Flag for zooming check
		cmp	#02				;Zoom?
		ifeq
			dey
		endif
		sty	temp3				;Save Flag
		lda	widthl
		sta	temp1
		lda	widthh
		lsr	A				;Need half of width later
		sta	temp1+1			;Will be used later
		ror	temp1
		lda	landsl
		ldx	landsh			;Use normal sight
		ldy	mzgame			;Zooming??
		cpy	#02
		ifeq					;yes.... so ...
			lda	shipxl
			ldx	shipxh			;Use ships position
		endif
		sta	xcomp				
		stx	xcomp+1			;Store X Position
		jsr	cor3p2			;Third person correction and draw (xonly)
		jsr	sclst3			;Restore scale to platform size
		lda	#00
		sta	xcomp+2
		sta	xcomp+3			;Now draw to left of beacon
		jsr	dblneg			;Want vector to the left
		sta	xcomp+1			;MSB 1/2 width
		lda	temp1
		sta	xcomp				;1/2 width
		jsr	vgvtr2			;Position to left beacon
		jsr	sclst2			;Scale of beacon set
		ldy	#00				;Beacon offset amount
		lda	#$0B				;Red color
		jsr	beacon
		jsr	sclst3			;Reset Scale
		lda	widthl
		sta	xcomp
		lda	widthh
		sta	xcomp+1			;Now draw a line to other beacon
		lda	frame				
		rol	A
		rol	A
		rol	A
		and	#$F0
		ora 	#whtlin			;Color of landing platform
		ldx	#$60
		jsr	vgadd2
		lda	#$20				;Vector intensity
		sta	vgbrit
		jsr	vgvtr2
		jsr	sclst2			;Scale of this beacon
		ldy	#$18				;Beacon Number
		lda	#$0B				;Red Color
		jsr	beacon
		jmp	bguns
		
;*************************************************
	.sbttl "First Person Motion and Picture"
;*************************************************
;* Space Station 1st Person Routine              *
;*                                               *
;* Uses:	A,X,Y	                               *
;* Stack:	2 bytes                              *
;*************************************************
statfb	lda	lasst				;Laser On?
		ifeq					;Skip if laser on
			lda	#$20
			bit	stbflg			;H Motion
			ifne
				lda	#00
				sta	temp1+1			;H Speed
				lda	#$1F
				ldy	maznum
				cpy	#spacefort
				ifeq					;Game 0 uses it's own speed
					lda	saucvl
				endif
				sta	temp1
				lda	#$10				;Check this bit
				bit	stbflg			;Left or Right?
				ifeq
					jsr	dblneg			;Left
				endif
				clc
				lda	temp1				;LSB Speed
				adc	statxl
				sta	statxl
				tay					;Save copy
				lda	statxh
				adc	temp1+1
				sta	statxh			;MSB Add
			endif
		endif
		bit	stbflg			;Vertical Motion Hold?
		ifmi					;No... Do motion
			ldx	mzgame			;If not space, skip the hold motion
			bne	?stb6
			ldx	lasst				;Laser Active??
			bne	?stb5				;If yes, skip motion
?stb6			ldy	#$18				;Fly by speed
			lda	maznum			;Space fort game??
			cmp	#spacefort
			ifeq
				lda	mzgame			;But only in space
				ifeq
					lda	saucvl
					lsr	A
					tay
				endif
			endif
			sty	temp1
			ldy	#00			;Guess this speed (moving down screen)
			sty	temp1+1
			ifvs				;VC is move
				jsr	dblneg		;Moving the other way
			endif
			clc
			lda	statyl
			adc	temp1
			sta	statyl
			tax				;Save copy LSB
			lda	statyh
			adc	temp1+1
			sta	statyh
?stb5			ifmi				;If - , went off top of screen
				lda	#00
				sta	statst		;Turn it off for now
				sta	stbflg		;Also clear flag too
				sta	statyh
				sta	statyl		;Stick at 0
				rts				;No need to draw it either
			else
				lda	statyh		;In case we lost it
				cmp	#$0B			;See if it hit bottom
				ifcs
					lda	#08
					ldy	shipst		;We alive?
					ifeq
						lda	#$10			;If not go here
					endif
					sta	mzgame		;Next game
					lda	#stthird
					sta	shppic		;First trans pic
		;----------------- 3rd Person Conversion ------------------------------------
					lda	#$B8
					sta	shipyl
					lda	#$0B
					sta	shipyh
				endif
			endif
		endif
		bit	stbflg			;Moving up screen?
		ifvs					;Yes
			cmp	#04				;Launch Fighter Point
			ifeq					;yep
				ldy	#$80				;Okay to launch fighters
				sty	lauen
			endif
		endif
		ldx	#03
		begin
			lda	statxl,X
			sta	xcomp,X			;Put position into xcomp
			dex
		miend
		ldx	maznum			;Scale Control
		lda	sclsb,X			;Correction LSB
		sta	scalef
		lda	scmsb,X
		sta	scalef+1			;Size offset
		jsr	posvec			;Ok mark, Place this and set scale
		ifmi
			rts					;Skip if not drawn
		endif
		
fpbase	;Entry from 3rd person special view for blowship!!
		ldy	#$F2				;Guess Green
		lda	blowst
		and	#08				;White?
		ifne
			ldy	#$F8
		endif
		tya
		ldx	#bas7				;Stat, page select (other base stations)
		jsr	vgadd2			;Add stat
		lda	maznum			;Must recall number
		asl	A
		tay	
		lda	fbase,Y			;Get the pic
		ldx	fbase+1,Y
		jmp	vgadd2			;Draw it!!
		
;*********************************************************
;* ychk - Used to calculate differnce between ship and   *
;*        station. Used more than once.                  *
;*********************************************************
	.sbttl "Y Difference Check"
;*********************************************************
ychk		lda	statyl			;Y LSB saved above (statyl)
		sec	
		sbc	shipyl
		sta	temp1				;For abs possibility
		tya	
		sbc	shipyh
		sta	temp1+1
		rts	
		
;*********************************************************
;*  Landit - Called above to check for a good landing    *
;*                                                       *
;*  (width/2)-(shipsize/2)-ABS(difference) > 0 ??        *
;*  (width-size-2*dif) > 0 ??                            *
;*  shipsize+(2*diff)-width < 0 ??                       *
;*********************************************************

shpszl	=	$38
shpszh	=	$0

landit	lda	shipst			
		ifne
			ifpl					;Not blowing up or dead
				lda	#$80
				sta	statst			;Stop Motion
				lda	shipxl			;Ship/Station Diff saved above
				sec
				sbc	landsl			;Landing point
				sta	temp1
				lda	shipxh
				sbc	landsh			;MSB Landing Point
				sta	temp1+1
				ifmi
					jsr	dblneg			;Abs value
				endif
				
			; (A) = MSB Difference
			; Above checks with center, so must check against half the
			; width... To do this I will multiply the difference by 2
			; (instead of dividing the width by 2).
				asl	temp1				;LSB * 2
				rol	A				;MSB * 2 prop carry
				sta	temp1+1			;2* difference (MSB)
				clc
				lda	temp1
				adc	#shpszl			;LSB of ship size
				sta	temp1
				lda	temp1+1
				adc	#shpszh			;MSB of size
				sta	temp1+1
			; (2 * ABS(diff))+shipsize in temp1(2)
				lda	temp1
				sec	
				sbc	widthl			;Compare to the width
				lda	temp1+1
				sbc	widthh			;Check MSB width
				ifcc
					lda	#02
					sta	mzgame			;Set game play to zoom
					lda	#snd_h1			
					jsr	dosound
					lda	#$8A
					sta	mtim
				else					;Bad landing... BOOM!
?ldi30				lda	#tiltpic
					sta	shppic
					jsr	blowship			;Into peices
					ldx	#$0F				;Fill color RAM with all white
					lda	#white			;White
					begin
						sta	colram+$10,X
						dex
					miend					;So ship stays white
					jsr	dostop			;Stop and let it be done
					lda	#snd_stm			
					jsr	dosound
					lda	#snd_h2
					jsr	dosound
				endif
			endif
		endif
		rts
		
	.sbttl "Third Person View Vector Correction"
;Places 0,0 in the upper left corner and scale 0 (I hope)	
cor3p		jsr	cor3
cor		lda	#00
		sta	vgbrit
		jsr	vgcntr			;Center
		jsr	sclset			;Set 3rd Person Scale
		jmp	vgvtr2			;And draw it!
cor3p2	jsr	cor32				;Do X only
		jmp	cor
cor3		lda	#$80
		sec	
		sbc	xcomp+2			;Y runs wrong direction
		sta	xcomp+2
		lda	#05				;Neg(x-5)
		sbc	xcomp+3
		sta	xcomp+3
		;---------------- temp correction to put x to a.ff on screen ----------------------
cor33		cmp	#$80
		ror	A
		ror	xcomp+2
		cmp	#$80
		ror	A
		ror	xcomp+2
		sta	xcomp+3			;y/4
cor32		lda	xcomp
		sec	
		sbc	#$80
		sta	xcomp
		lda	xcomp+1
		sbc	#center			
		sta	xcomp+1			;Corrected X
		rts	
		
;***************************************************************
	.sbttl "Set Scales for Position"
;***************************************************************
;* Set scale routines for above.. saves 4 bytes each routine.
;***************************************************************
sclst2	lda	#$60				;Beacon Size
		ldx	#$72				;Scale for beacon
		jmp	vgadd2
sclst3	ldx	mzgame			;Position during zoom
		cpx	#02				;zoom?
		ifeq					;Special scale for zoom in/out
			lda	shpscl
			sec	
			sbc	#$70				;It to get larger
			ifmi
				sec	
				sbc	#$80
				clc
			endif	
			tay	
			lda	shpsch
			sbc	#00			;Gets larger
			cmp	#$70
			ifcc				;Can't do this!!
				ldy	#00
				lda	#$70			;just stick at large
			endif
			tax	
			tya	
			inx	
			jmp	vgadd2
		endif					
sclset	lda	#00				;Else use normal draw
		ldx	#$72
		jmp	vgadd2			;Scale for position
		
;****************************************************************
	.sbttl "Maze Guns"
;****************************************************************
;* This is used to display and move the guns on the top of the  *
;* mazes. They are used when a ship tries to pass off the edge  *
;* of the maze.                                                 *
;****************************************************************
bguns		lda	mzgame
		cmp	#02
		ifeq					;Skip during zoom
			rts	
		endif
		lda	#01				;Do 2 guns
		sta	temp3
		begin
			lda	statyl
			sta	xcomp+2			;Gun Y position
			lda	statyh
			sta	xcomp+3
			ldx	temp3
			lda	frame				;Open close rate
			and	#03
			ifeq
				lda	gunctl,X			;If 0, leave alone
				ifne
					ifpl					;- but says close
						clc	
						adc	#02
						cmp	#$0B				;Open?
						bcs	?bgu10			;If yes, just skip this
					else					;Else, close the thing
						sec	
						sbc	#02
						ifpl					;Closed already?
							lda	#00				;Set back to normal
						endif
					endif
					sta	gunctl,X			;Restore control
?bgu10			endif
			endif
			lda	maznum
			asl	a				;2 entries per maze
			tax	
			lda	temp3
			ifne
				inx					;Use second entry
			endif
			lda	statxl
			clc	
			adc	gunxl,X
			sta	xcomp
			lda	statxh
			adc	gunxh,X
			sta	xcomp+1
			ldx	temp3				;Recall index
			ldy	gunctl,X
			ifne					;This one is on
				cpy	#01				;Test Y for which gun
				ifeq
					ldy	#00				;Move laser start over right
					lda	#$20
				else
					ldy	#-1				;Else move it left
					lda	#-$20
				endif
				clc	
				adc	xcomp
				sta	lsdsxl
				tya	
				adc	xcomp+1
				sta	lsdsxh
				lda	statyl			;Origin correction
				sec	
				sbc	#$30
				sta	lsdsyl
				lda	statyh
				sbc	#00
				sta	lsdsyh
			endif
			lda	#00
			ldx	#gun7				;Stat, page select
			jsr	vgadd2			;place gun normal stat
			jsr	cor3p				;correct for 3rd person
			lda	#00
			ldx	#$71
			jsr	vgadd2			;Scale of gun
			lda	#$F1
			ldx	#gun7				;stat, page select
			ldy	temp3
			ifeq
				tay					;Save A
				txa	
				ora	#xflip			;Add flip
				tax
				tya					;restore A
			endif
			stx	temp3+1			;Save stat X for later
			jsr	vgadd2			;Add stat
			ldx	temp3				;Which gun
			lda	gunctl,X			;Gun control for this gun
			and	#$7F				;Drop control bit
			tay	
			pha					;Save copy of index
			lda	heads,Y
			ldx	heads+1,Y			;Get proper head pic
			jsr	vgadd2			
			lda	#02
			ldx	#00
			ldy	#00
			jsr	vgvtr5
			lda	#$FA
			ldx	temp3+1			;Recall flip for new color
			jsr	vgadd2
			lda	#$40
			ldx	#$71
			jsr	vgadd2			;Scale of gun
			pla	
			sec	
			sbc	#04				;Delay open
			ifpl
				cmp	#06
				ifcs					;If greater, always use 6
					lda	lasst				;Laser On??
					ifeq					;no
						ldx	#03
						begin					;Fire laser
							lda	shipxl,X
							sta	lstsxl,X			;destination
							dex
						miend
						lda	lstsyl
						sec	
						sbc	#$80
						sta	lstsyl
						lda	lstsyh
						sbc	#00
						sta	lstsyh			;Offset to center of ship
						lda	shipst			;Ship Active??
						ifne
							ifpl
								lda	gunctl
								ora	gunctl+1			;Assumed only one working
								cmp	#$0A				;Open and ready to fire
								ifeq
									lda	#$EF				;Fire!!!
									sta	lasst				;Turn it on
									lda	#07
									sta	laspd				;Fast Laser
								endif
							endif
						endif
					endif
					lda	#06
				endif
				tay
				lda	guns,Y
				ldx	guns+1,Y
				jsr	vgadd2
			endif
			dec	temp3
		miend
		rts
		
;********************************************
	.sbttl "Bump Game"
;********************************************
;* Used to advance and set up the game for  *
;* the maze play portion from the space     *
;* portion.                                 *
;********************************************	
bumpgame	lda	#$80
		sta	restart			;Hold this picture
		sta	ymot				;Set man's postion at ship
		sta	objyl
		jsr	stpg1				;Select page 1
		jsr	map				;Draw map on top of screen
		jsr	stpg0				;Reset to page 0
		lda	#-1				
		sta	xmot+1
		lda	#00
		sta	ymot+1
		sta	piccur			;Make him run
		sta	tspark
		sta	nodraw			;Clear these just in case
		lda	#$DA
		sta	xmot
		lda	#$FC
		sta	mazeyh			;Place man/maze on screen
		ldx	maznum			;Different vel for different mazes
		lda	tossvel,X
		sta	velxh				;'throw' him out of space ship
		lda	#$80
		sta	objxl
		sta	direct			;Jump out of ship right direction
		lda	#07
		sta	objxh
		lda	#$82
		sta	mzgame			;Set to maze play
		; The 2 is so maze does not scroll right away (flag to motion)
		lda	gamest
		ora	#$18				;Add just entering bit
		sta	gamest
		lda	#mazcol+$E0
		sta	mapbuf+2			;Turn on map
		lda	#timcol+$E0
		sta	timbuf+2			;Turn on clock
		bit	atflag			;Attract flag??
		ifmi
			lda	#01
			sta	manstat			;Make sure on
		endif
		rts
			
tossvel	.byte -$40,-$30,-$28,-$28

;****************************
;* Table of base Ship Sizes *
;****************************
mazxsh	.byte 1,1,0,0			;Maze Size MSB
mazxsl	.byte 4,2,$E2,$D8			;LSB Size

;*****************************
;* Scale correction for each *
;* ship draw on 1st person.  *
;*****************************
sclsb		.byte $7F,$40,$00,$00
scmsb		.byte $00,$01,$00,$01

;*****************************
;* Gun positions for bases.  *
;*****************************
gunxl		.byte $48,$B8,$4A,$B6,$68,$98,$78,$88
gunxh		.byte -1,0,-1,0,-1,0,-1,0


;***********************************************
	.sbttl "Position a ship on the grid"
;***********************************************
;* Input:	xcomp  = x lsb  (         )        *
;*		xcomp+1= x msb  (unchanged)        *
;*         	xcomp+2= y lsb  (         )        *
;*          xcomp+3= y msb  (         )        *
;*                                             *
;* Output:	Adds up to 20d bytes to vglist     *
;*		z=1,m=0 Object Displayed on Grid   *
;*          m=1,z=0 Object off grid            *
;*                                             *
;* Uses:	temp1,temp2,temp3,temp4,temp5      *
;***********************************************
posvec	lda	xcomp+3
		cmp	#$0B
		bcs	offscreen
		lda	xcomp+1
		cmp	#09
		bcs	offscreen
		lda	xcomp+3
		cmp	#$0A
		bne	onscreen
		lda	xcomp+1
		cmp	#02
		bcc	offscreen
		cmp	#07
		bcc	onscreen
offscreen	lda	#$FF				;Vector out of bounds
		rts
		
posvc2	
onscreen	ldy	#00
		lda	#$40
		sta	(vglist,Y)
		iny	
		lda	#$80
		sta	(vglist,Y)
		iny	
		lda	#00
		sta	(vglist,Y)
		iny	
		lda	#$71
		sta	(vglist,Y)
		iny	
		lda	#00
		sta	(vglist,Y)
		iny	
		lda	#$60
		sta	(vglist,Y)
		jsr	vgadd				;scal 0,0
		lda	xcomp+3
		asl	a
		tay	
		sta	temp5
		lda	gridy,Y
		ldx	gridy+1,Y
		jsr	vgadd2			;vctr, delta Y and opcode
		lda	xcomp+1
		asl	a
		tay	
		lda	xcomp				;Twice as above
		ifmi					;X units as Y
			iny	
		endif
		sty	temp1				;X bin
		asl	temp1
		lda	xcomp+3
		asl	a
		asl	a
		sta	temp2
		asl	a
		asl	a
		adc	temp2
		ldx	temp1
		cpx	#$14
		ifcc
			adc	temp1
			tay	
			lda	gridx2,Y
			sbc	gridx,Y
			sta	temp4
			lda	gridx2+1,Y
			sbc	gridx+1,Y
			and	#$1F
			sta	temp4+1
			lda	gridx,Y
			ldx	gridx+1,Y
		else
			adc	#$23
			sec	
			sbc	temp1
			tay	
			sec	
			lda	gridx,Y
			sbc	gridx2,Y
			sta	temp4
			lda	gridx+1,Y
			sbc	gridx2+1,Y
			and	#$1F
			sta	temp4+1
			lda	#00
			sec	
			sbc	gridx,Y
			pha	
			lda	#00
			sbc	gridx+1,Y
			and	#$1F
			tax	
			pla	
		endif
		jsr	vgadd2
		lda	xcomp+2
		lsr	A
		lsr	A
		ifeq
			lda	xcomp
			asl	a
			beq	?psv10
			ldy	#00
			sta	temp1
			lda	#00
			sec	
			sbc	temp1
			sta	(vglist,Y)
			iny	
			lda	#$71
			sta	(vglist,Y)
			iny	
			ldx	xcomp+3
			jmp	?psv5
		endif
		tax	
		ldy	#00
		lda	qrtlog-1,X
		sta	(vglist,Y)
		iny	
		lda	#00
		sec	
		sbc	fullog2-1,X
		sta	temp1
		lda	#$71
		sta	(vglist,Y)
		iny	
		ldx	temp5
		lda	disty,X
		sta	(vglist,Y)
		iny	
		lda	disty+1,X
		sta	(vglist,Y)
		iny	
		lda	temp4
		sta	(vglist,Y)
		iny	
		lda	temp4+1
		sta	(vglist,Y)
		jsr	vgadd
		lda	xcomp
		and	#$7F
		ifne
			jsr	multiply
			asl	a
			rol	temp2+1
			ldx	#$71
			lda	#00
			sec	
			sbc	temp2+1
			ifne
				jsr	vgadd2
				ldy	#00
				ldx	xcomp+3
				inx	
?psv5				lda	#00
				sta	(vglist,Y)
				iny	
				lda	#00
				sta	(vglist,Y)
				iny	
				lda	distx,X
				sta	(vglist,Y)
				iny	
				lda	#00
				sta	(vglist,Y)
				jsr	vgadd
			endif
		endif
?psv10	lda	scalef+1,abs
		cmp	#$80
		ifeq
			lda	xcomp+3
			cmp	#$0A
			ifcc
				sta	temp1
				lda	xcomp+2
				lsr	temp1
				ror	A
				lsr	A
				tay	
				lda	#05
				sec	
				sbc	temp1
				ora	#$70
				tax	
				lda	fullog,Y
			else
				lda	#00
				ldx	#$71
			endif
			jsr	vgadd2
		else
			cmp	#$40
			ifne
				clc	
				lda	xcomp+2
				adc	scalef,abs
				tay	
				lda	xcomp+3
				adc	scalef+1,abs
				clc	
				sta	temp1
				ifmi
					sec
				endif	
				tya	
				ror	temp1
				ror	A
				lsr	A
				tax	
				ldy	fullog,X
				lda	#05
				sec	
				sbc	temp1
				ifmi
					lda	#00
					tay	
				endif
				cmp	#06
				ifcs
					lda	#06
				endif
				clc	
				adc	#01
				jsr	vgscal
			endif
		endif
		lda	#00
		rts	
		
distx		.byte $02,$03,$04,$06,$08,$0B,$10,$17,$20,$2D,$40,$5B

disty		.word $1FFA,$1FF7,$1FF4,$1FEE,$1FE7,$1FDD,$1FCE,$1FBA,$1F9D,$1F73,$1F39

gridx		.byte $EE,$1F,$F0,$1F,$F2,$1F,$F4,$1F,$F6,$1F,$F8,$1F,$FA,$1F,$FC,$1F,$FE,$1F,$00,$00
gridx2	.byte $E7,$1F,$E9,$1F,$EC,$1F,$EF,$1F,$F2,$1F,$F5,$1F,$F8,$1F,$FA,$1F,$FD,$1F,$00,$00
		.byte $DC,$1F,$E0,$1F,$E4,$1F,$E8,$1F,$EC,$1F,$F0,$1F,$F4,$1F,$F8,$1F,$FC,$1F,$00,$00
		.byte $CD,$1F,$D3,$1F,$D8,$1F,$DE,$1F,$E4,$1F,$E9,$1F,$EF,$1F,$F5,$1F,$FA,$1F,$00,$00
		.byte $B8,$1F,$C0,$1F,$C8,$1F,$D0,$1F,$D8,$1F,$E0,$1F,$E8,$1F,$F0,$1F,$F8,$1F,$00,$00
		.byte $9A,$1F,$A5,$1F,$B1,$1F,$BC,$1F,$C7,$1F,$D3,$1F,$DE,$1F,$E9,$1F,$F5,$1F,$00,$00
		.byte $70,$1F,$80,$1F,$90,$1F,$A0,$1F,$B0,$1F,$C0,$1F,$D0,$1F,$E0,$1F,$F0,$1F,$00,$00
		.byte $34,$1F,$4B,$1F,$62,$1F,$78,$1F,$8F,$1F,$A5,$1F,$BC,$1F,$D3,$1F,$E9,$1F,$00,$00
		.byte $E0,$1E,$00,$1F,$20,$1F,$40,$1F,$60,$1F,$80,$1F,$A0,$1F,$C0,$1F,$E0,$1F,$00,$00
		.byte $69,$1E,$96,$1E,$C3,$1E,$F0,$1E,$1E,$1F,$4B,$1F,$78,$1F,$A5,$1F,$D3,$1F,$00,$00
		.byte $C0,$1D,$00,$1E,$40,$1E,$80,$1E,$C0,$1E,$00,$1F,$40,$1F,$80,$1F,$C0,$1F,$00,$00		
		.byte $D1,$1C,$2C,$1D,$86,$1D,$E1,$1D,$3B,$1E,$96,$1E,$F0,$1E,$4B,$1F,$A5,$1F,$00,$00
		
gridy       .word $00E1,$00DB,$00D2,$00C6,$00B4,$009B,$0078,$0046,$0000,$1F9D,$1F10,$1E49

;*****************************************
	.sbttl "Tactical Display Routines"
;*****************************************
;* Entry just after ship is drawn, scale *
;* previously set.                       *
;*****************************************
;* Message levels (places for text message)
;* (Recall these are -1 from displayed level)

mesl1	=	4
mesl2	=	10
mesl3	= 	13
mesl4	=	15

tactdi	ldy	#$FF
		ldx	dif4mz
		cpx	#mesl4+1			;Skip Everywhere??
		ifcc					;Nope
			ldy	teltab,X			;What to tell
			cpy	wrplvl
			ifcc
				ldy	#$FF			;Don't tell player about warp if he used it already
			endif
		endif
		cpx	#$0C
		ifcs
			lda	#$80
			sta	diedyet
		endif					;No warping after beginning of last cycle
		sty	perm1
		lda	#$A7
		ldx	#tact7			;Set page
		jsr	vgadd2
		laljsr(tactd)
		lxhjsr(tactd)			;JSRL to display
		jsr	vgadd2
		lda	shpsch			;Scale of this display
		cmp	#$73				;Hold rest until out
		ifne
			rts				;Skip until small
		endif
		;Now add storage screen object
		jsr	vgcntr
		lda	seqst				;Object Status
		and	#$20				;Erase bit??
		ifne
			lda	frame
			and	#07
			ifeq
				lda	#00
				sta	seqx
				sta	seqst				;Done with erase
				beq	nextd				;Skip here and do next
			endif
			lda	#$F7				;Bright to erase
		else
			lda	#$A7
		endif
		ldx	#$60				;In 'light white'
		jsr	vgadd2
		lda	#$7F
		ldx	#$44				;Position
		jsr	vgvtr5
		lda	#$22
		ldx	#00				;At this scale, 2 vectors to get there
		jsr	vgvtr5
		lda	seqp				;Which picture
		and	#03
		asl	a				;Word entries
		tax	
		jsr	copypic			;Do display
;* Time for Heart Scanner *
nextd		jsr	vgcntr
		lda	#$A6
		ldx	#$60
		jsr	vgadd2			;Color
		lda	#00
		ldx	#$72				;This may be Scale 2
		jsr	vgadd2
		lda	#$40
		ldx	#-6
		jsr	vgvtr5			;Position
		lda	condition			;Which one?
		tax					;Save in X
		lsr	A
		tay	
		lda	hsbox,Y			;Get color for this
		sta	colram+4			;In this location
		jsr	hscand			;Display it
		jsr	lrscan			;Do long range scanner
		ldx	#mcont
		jsr	mesg				;Condition Message
		lda	brstat			;Playing??
		ifne					;Button was pressed
			bit	perm1				;Warp Display Wave?
			ifmi					;Yes if -, (else go to hell!)
				ldx	#mgarbage
				jsr	mesg
			endif
		endif
		lda	frame
		and	#$0F				;Seconds
		ifeq
			inc	ttime
		endif
		lda	ttime
		cmp	#30d				;30 half seconds??
		bcs	?tdp10
		cmp	#12d				;Longer enough??
		ifcs					;yes!
			ldx	perm1				;Warp Wave??
			ifmi					;Yep, full display time always
				bit	brstat			;Playing breakout?
				ifpl					;no
?tdp10				lda	#$40
					sta	tact				;End Wave
					lda	#00
					sta	tactde			;Turn off enable for next time
					sta	targf				;Tell mainline not at target anymore (kludge)
					ldx	dif4mz
					lda	expdth,X
					sec
					sbc	incded
					ifpl
						ifne
							clc
							adc	incdif
							cmp	#04
							ifcs
								lda	#04
							endif
							ldx	addap
							ifne
								sta	incdif
							endif
						endif
					endif
					lda	dif4mz
					cmp	#02
					ifcs
						lda	#00
						sta	incded
					endif
					jsr	dostop
					lda	ttime
					cmp	#13d				;If leaving because of breakout
					ifcs
						lda	#snd_b2d
						jsr	dosound			;Must have used breakout to get here
					endif
				endif
			endif
		endif
		lda	condition
		cmp	#04				;Don't go past 4
		ifcc
			ldx	ttime
			lda	condtbl,X			;What condition here
			sta	condition
		endif
		lsr	A
		tax	
		lda	tmess,X
		tax	
		jsr	mesg				;Condition message
		lda	sndcue,abs
		ifeq
			lda	#$80
			sta	sndcue,abs
			lda	#snd_b1a
			jsr	dosound
		endif
		lda	condition
		cmp	#04				;Red alert?
		ifeq
			lda	frame
			and	#$7F
			ifeq
				lda	#snd_b1e
				jsr	dosound
			endif
		endif
		lda	seqst
		ifeq
			lda	frame
			and	#$0F
			ifeq
				lda	seqp
				cmp	maznum			;Same pic?
				ifne
					inc	seqp
					lda	seqp
					and	#03
					sta	seqp
				endif
				inc	seqst
			endif
			lda	seqst				;Restore signs
		endif
		ifmi
			lda	seqp
			cmp	maznum			;Same pic??
			ifeq
				ldx	maznum
				lda	etypm,X			;Enemy Type message
				tax	
				jsr	mesg
				bit	frame
				ifvs
					ldx	#mconf			;Confirmed message
					jsr	mesg
				endif
			else
				lda	frame
				and	#$1F
				cmp	#$1C				;Diff is erase flash time
				ifeq
					lda	#$A0
					sta	seqst
				endif
			endif
		endif
		ldx	#mmdist
		jsr	mesg				;Distance Message
		lda	#$C0
		sec	
		sbc	lroff				;Get Distance
		jsr	decimal
		ldy	#02
		lda	#temp7
		sec	
		jsr	digits			;Display distance
		lda	frame
		and	#08
		ifne
			ldx	#mcols			;Guess Closing
			lda	lroff
			cmp	#$A0
			ifcs					;Nope, Holding
				ldx	#mhold
			endif
			jsr	mesg
		endif
		ldx	#metyp
		jsr	mesg
		ldx	#mmlevel			;Tell Level
		jsr	mesg
		lda	difcty			;Level is difcty*4+maznum
		asl	a
		asl	a
		sec	
		adc	maznum
		jsr	decimal			;In decimal
		ldy	#01
		lda	#temp7
		jsr	digits
		jsr	vgcntr
		lda	#$FA
		ldx	#$60
		jsr	vgadd2			;Color
		lda	#00
		ldx	#$72
		jsr	vgadd2			;Size
		lda	#$40
		ldx	#08
		jsr	vgvtr5			;Position
		lda	frame
		and	#$0F
		ifeq
			lda	rands
			sta	tactln,abs
		endif
		lda	tactln,abs
		and	#$17
		ldy	#$20				;Use Stat
		ldx	#00				;No Y deflection
		jsr	vgvtr				;Display time left line (above heart scan)
		ldx	perm1				;Display Wave??
		ifpl
			lda	frame
			lsr	A
			lsr	A
			ora	#$E0
			sta	colram+$0F			;Flash color for this
			ldx	#mwarp0
			jsr	mesg
			ldy	perm1
			ldx	whathint,Y
			jsr	mesg				;Put up warp info
		else
			ldy	maznum
			ldx	tmesg,Y			;Get message this wave
			jsr	mesg
		endif
		
	.sbttl "Center Scanner Pic Control"
cscan		lda	#00
		ldx	#$73				;Scale for window vector
		jsr	vgadd2
		jsr	vgcntr
		lda	#00
		ldx	#$7F
		jsr	vgvtr5			;Position for window line
		lda	#00
		ldx	#$79
		jsr	vgadd2			;Add window at top of box
		jsr	vgcntr			;Recenter
		lda	#$C0
		sec					;Position is 0C0-lroff (0A0 Max)
		sbc	lroff				;Get position
		sta	xcomp+2
		lda	#00
		sta	xcomp
		sta	xcomp+1
		sta	xcomp+3			;zero X and Y MSB
		jsr	vgvtr2			;Draw vector
		lda	#$E2				;Color and page select of pic
		ldx	#tacct7
		jsr	vgadd2
		lda	#00
		ldx	#$73				;Scale of center pic
		jsr	vgadd2
		lda	maznum			;Get correct pic
		asl	a
		tay					;*2 in Y for table lookup
		lda	tcn,Y
		ldx	tcn+1,Y			;Get pic
		jsr	vgadd2			;Add pic
		lda	#00
		ldx	#$71				;Reset scale
		jsr	vgadd2			
		lda	#00
		ldx	#$7F
		jsr	vgvtr5			;Move beam way up again
		lda	#00
		ldx	#$79				;Put beam killer way at top again
		jsr	vgadd2

;* Get Rolly-Gig data for warp code.....

		lda	wrpwh				;Skip it, bad digits entered??
		ora	diedyet			;Or player died or too high a wave??
		ifmi
			rts					;yep!
		endif
		ldx	dif4mz
		ldy	whichw,X			;Warp 0,1,or 2
		sty	temp9+1
		lda	wdigits,Y
		sta	perm2				;2 or 3 digits to the code
		lda	warpc0,Y
		sta	perm2+1			;Warp code in perm2+1 to perm3+1
		lda	warpc1,Y
		sta	perm3
		lda	warpc2,Y			;If no third digit, code must be 0
		sta	perm3+1
		ldx	whatmes,Y
		jsr	mesg				;Display 'color' warp message
		ldx	wrpwh
		cpx	perm2				;All digits entered?
		ifeq
			jsr	compcod			;Compare codes
			bne	?rgw10
			ldy	temp9+1
			iny	
			sty	wrplvl			;Store away warp level
			dey	
			ldx	whered,Y			;New difficulty level
			lda	wherem,Y			;New maznum
			jsr	thisone			;Bump you one difcty
			lda	#snd_b3b
			jsr	dosound
			ldx	dif4mz
			lda	expdth,X
			sta	incded
			sed	
			lda	warps
			clc	
			adc	#01				;Another warp taken
			sta	warps
			lda	warps+1
			adc	#00
			ifcc
				sta	warps+1			;Don't wrap
			endif
			cld	
			ldx	#00
			lda	player,abs			;Who is this?
			ifne					;It's player 2
				ldx	#score2-score		;Index to score 2
			endif
			ldy	temp9+1
			lda	warpts,Y
			sta	score+2,X			;Bonus points
			lda	#00
			sta	score+1,X
			sta	score,X
			ldy	temp9+1
			lda	whatliv,Y
			ifne
				jsr	addlif			;Player gets another life
			endif
			lda	#$80
			sta	scoflg			;Indicate change
?rgw10	else
			ldy	#00
			lda	rgdd
			asl	a
			asl	a
			ifmi
				ldy	#$99
			endif
			clc	
			adc	wrpdl
			sta	wrpdl
			sed	
			tya	
			adc	wrpdat,X
			and	#$0F
			sta	wrpdat,X
			cld	
			bit	jblast			;Last on or off?
			ifmi					;Was off
				bit	jbstat			;Jump to lock
				ifpl
					ldx	wrpwh
					lda	perm2+1,X
					cmp	wrpdat,X
					ifeq
						lda	#00
						sta	ttime			;Give Him/her more time to enter second digit
					endif
					inc	wrpwh
					ldx	wrpwh
					cpx	perm2
					ifne
?rgw20					lda	#snd_b3a			;Digit Entered
					else
						jsr	compcod			;All digits entered, correct code?
						bne	?rgw20
						lda	#snd_b3b
					endif
					jsr	dosound
				endif
			endif
		endif
		lda	jbstat
		sta	jblast			;Store last
		lda	#00
		sta	perm4				;Draw 2 or 3 digit code
		begin
			cmp	wrpwh
			ifcs
				lda	#$F7				;White before entered
			else
				ldy	temp9+1			;Otherwise, warp code color instead
				lda	warpcol,Y
			endif
			ldx	#$60
			jsr	vgadd2			;Alphanumerics in 5000 page
			ldy	perm4
			lda	wrpdat,Y
			jsr	vghex				;And the digit
			inc	perm4
			lda	perm4
			cmp	perm2
		eqend
		rts	
		
teltab	.byte -1,-1,-1,-1
		.byte  0,-1,-1,-1
		.byte -1,-1, 1,-1
		.byte -1, 2,-1, 3	

whichw	.byte 0,0,0,1,  1,1,1,1,  2,3,3,3		;Which warp to use

wdigits	.byte 2,2,3,3					;Number if digits in warp

whered	.byte 0,2,2,3					;Which difcty to go to
wherem	.byte 3,0,1,0					;Which maze to go to

warpcol	.byte $8B,$C6,$C2,$F3				;Color of numbers of warp
whatliv	.byte 0,0,0,1					;Extra Life?
whatmes	.byte mmred,mmyel,mmgrn,mmaqu			;Warp message
warpc0	.byte 2,4,8,3					
warpc1	.byte 3,6,2,1
warpc2	.byte 0,0,4,5
warpts	.byte $25,$60,$70,$95

compcod	begin
			lda	perm2+1,X
			cmp	wrpdat,X			;Compare entered data to warp code
			bne	?cod10			;Bad digit, no action
			dex
		miend					;All digits good
		lda	#00
?cod10	rts

;************************************************
	.sbttl "Long Range Scanner"
;************************************************
lrscan	lda	maznum
		asl	a
		asl	a
		clc	
		adc	#03
		tay					;Input Place
		ldx	#03
		begin
			lda	lrvctr,Y			;copy base position to xcomp
			sta	xcomp,X			;store Y position
			dey	
			dex	
		miend
		lda	lroff
		cmp	#$A0
		ifcc
			inc	lroff
		endif
		lda	lroff
		lsr	A
		lsr	A
		clc	
		adc	xcomp+2
		sta	xcomp+2
		lda	xcomp+3
		adc	#00
		sta	xcomp+3
		jsr	vgcntr
		jsr	vgvtr2			;Position for draw
		lda	#$A6				;yellow
		ldx	#tact7
		jsr	vgadd2			;Color of box
		lda	#$40
		ldx	#$72				;Size of box
		jsr	vgadd2
		laljsr(lrsrbx)
		lxhjsr(lrsrbx)			;Add box
		jmp	vgadd2			;Add picture
		
lrvctr	.word -$166,-$b0,-$166,-$50,-$166,$10,-$166,$70

etypm		.byte msphr,mfigh,mspin,mstar

condtbl	.byte 0,0,0,0,2,2,2,2,2,4

;*****************************************************
	.sbttl "List Sequencer"
;*****************************************************
;* This routine will be used to copy vectors from    *
;* VGROM to VGRAM, 1 at a time, making the last      *
;* drawn vector brighter.                            *
;* Limits: Ths source pics must be in the fixed      *
;*         vector page. No more than 3 of these      *
;*         allowed.                                  *
;*                                                   *
;* Inputs: X       = display select (see list below) *
;*         seqx(x) = how far already in list         *
;*         seqst(x)= 80 if whole pic achieved        *
;*                   40 if last vec was long         *
;*                   20 if erase mode on             *
;*****************************************************
copypic	lda	dislist,X			;Get address of source
		sta	temp2
		lda	dislist+1,X			;Get MSB
		sta	temp2+1
copy2	ldy	seqx				;Current vector working on
		lda	seqst				;Do this one??
		ifpl					;Yep, not all there
			lda	frame	
			and	#01
			ifeq				;Time to add another to list
				iny					
				lda	(temp2,Y)			;Get high byte of vector
				cmp	#$C0				;RTSL?
				ifeq
					dec	seqx				;And skip this on next time
					dec	seqx
					lda	#$80
					sta	seqst				;No more adds for this one
				endif
				jsr	add2_b			;Bump pointer by 2
				lda	seqst
				and	#$BF
				sta	seqst				;Guess short vector
				lda	(temp2,Y)			;Recall this opcode
				and	#$E0				;Look at opcode
				ifeq					;Must be long
					jsr	add2_b
					lda	seqst
					ora	#$40
					sta	seqst
				endif
			;Now point at next vector
			endif
		endif
		ldy	#00				;Start at start of list
		begin
			lda	(temp2,Y)
			sta	(vglist,Y)
			iny	
			cpy	seqx			;End of list
		csend
		dey	
		bit	seqst				;Skip bright if 'done'
		ifpl
			tya	
			pha					;Save this Y for later
			;Now point at last byte of opcode
			bit	seqst				;Long or short?
			ifvc					;Short
				dey
			endif
			lda	(vglist,Y)			;Get the vector	
			ora	#$E0				;make it bright
			sta	(vglist,Y)			;put it back
			pla					;restore old Y
			tay	
		endif
		jmp	vgadd				;Add Y to List
		
;************************************************
	.sbttl "Heart Scanner"
;************************************************
;* Heart Scan Monitor                           *
;*                                              *
;* Inputs: X    = which (1 of 4) to display     *
;*         hscan= how many svecs to draw blank  *
;************************************************
hscand	lda	frame
		and	#03
		ifeq
			inc	hscan
			inc	hscan
		endif
		lda	hlist,X
		sta	temp2
		lda	hlist+1,X
		sta	temp2+1			;Get input list of vectors
		ldy	#00
		begin
			lda	(temp2,Y)
			and	#$1F				;Turn off vector
			sta	(vglist,Y)
			iny	
			lda	(temp2,Y)
			cmp	#$C0				;An RTSL here?
			ifeq
				dey					;Back out the RTSL
				lda	#00
				sta	hscan				;Start over
				beq	?hsc10			;We hit end of list
			endif
			sta	(vglist,Y)			;Another short vector
			iny
			cpy	hscan				;Done enough?
		csend					;yep!
		ldx	#06				;Guess use 20
		lda	hscan
		cmp	#$0E				;Use all?
		ifcc
			lsr	A
			tax
		endif
		lda	stint,X			;Start Intensity
		sta	temp3				;Store intensity
		begin
			ldx	#01
			begin
				lda	(temp2,Y)
				ora	temp3
				sta	(vglist,Y)
				iny	
				lda	(temp2,Y)
				cmp	#$C0
				ifeq
					dey					;Back up before this one	
					jmp	?hsc10			;Force done
				endif
				sta	(vglist,Y)
				iny	
				dex					;Do 2 at each intensity
			miend
			lda	temp3
			clc	
			adc	#$20
			sta	temp3				;Continue until we wrap intensity
		csend					;We are done
?hsc10	dey	
		jsr	vgadd
		
;******************************************
	.sbttl "Breakout"
;******************************************
bdis		jsr	vgcntr
		lda	#00
		ldx	#$72
		jsr	vgadd2			;Working scale
		bit	bronce			;Did you get a life here??
		ifmi					;yep!
			lda	#$F1
			ldx	#$61				;***Shit... this needs a global
			jsr	vgadd2
			lda	#$51
			ldx	#-$28
			jsr	vgvtr5			;Position for rex
			lda	#$70
			ldx	#$71
			jsr	vgadd2			;Change scale
			laljsr(pic28)
			lxhjsr(pic28)
			jsr	vgadd2			;Draw Rex
			lda	#$F2
			ldx	#$D6
			jsr	vgvtr5			;Position for WOW
			lda	#$50
			ldx	#$71
			jsr	vgadd2			;Scale for letters
			lda	#$FC				;Flash color
			bit	perm5
			ifpl
				lda	#$F2				;Unless this happend in the distant past
			endif
			ldx	#$60
			jsr	vgadd2
			laljsr(char_w)
			lxhjsr(char_w)
			jsr	vgadd2			;W
			laljsr(char_o)
			lxhjsr(char_o)
			jsr	vgadd2			;O
			lda	#-1
			ldx	#00
			jsr	vgvtr5			;Compensate for Funky W
			laljsr(char_w)
			lxhjsr(char_w)
			jsr	vgadd2			;Last W
			lda	#00
			ldx	#$72
			jsr	vgadd2			;Restore Scale
			rts	
		endif					;So much for breakout
		lda	#02
		sta	temp3				;3 rows
		begin
			jsr	vgcntr
			ldy	temp3
			lda	#$41				;x place for all 3 rows
			ldx	yline,Y
			jsr	vgvtr5
			lda	#05				;5 bricks
			sta	temp3+1
			ldx	temp3
			lda	lincol,X
			ldx	#brick7			;Color and stat and page
			jsr	vgadd2
			ldx	temp3
			lda	brick,X			;1 byte each brick
			sta	temp2				;On or Off flags
			begin
				laljsr(brickp)
				lxhjsr(brickp)			;Guess will draw brick
				asl	temp2				;This one on?
				ifcc
					laljsr(brline)
					lxhjsr(brline)			;Skip over this one
				endif
				jsr	vgadd2
				dec	temp3+1			;Done all 6
			miend					;yes
			dec	temp3				;All 3 lines?
		miend
		
;* Fall through to ball Motion *

yline		.byte -$20,-$1C,-$18			;Position for Bricks

;*************************************
	.sbttl "Ball Motion"
;*************************************
ballm		lda	brstat			;Ball Moving??
		ifeq					;Waiting to serve
			jsr	getswitch
			ifpl					;He pushed it
				lda	#$80
				sta	brstat			;Start play, ball is slow
				lda	#-3
				sta	ballvx
				sta	ballvy			;Start moving down
				lda	#$C0
				sta	ballx
				sta	bally
				lda	#00
				sta	ballx+1
				sta	bally+1			;MSB's 0
				lda	#snd_b2a
				jsr	dosound
			endif
		endif
		lda	brstat
		and	#$10				;Moving fast??
		ifeq					;Not yet
			lda	colcnt			;How many paddle hits??
			cmp	#05				;Time for speed up?
			ifeq
				inc	colcnt			;Do it once only
				bit	ballvx
				ifmi
					dec	ballvx
				else
					inc	ballvx
				endif
			endif
			cmp	#09			;Time to set it back?
			ifeq
				inc	colcnt		;Only do it once
				bit	ballvx
				ifmi
					inc	ballvx
				else
					dec	ballvx
				endif
			endif
		endif
		ldy	#00				;Guess + velocity
		lda	ballvx			;Velocity
		ifmi
			dey					;Prop sign
		endif
		clc	
		adc	ballx				;Add to position LSB
		tax					;Save LSB
		tya	
		adc	ballx+1			;MSB
		ifmi					;Don't let it go off left edge
			jsr	chbx				;Change Velocity
			lda	#00
			tax					;Set ball at 0,0
		else					;Not negative
			cpx	#$78				;Off right??
			ifcs					;yep
				jsr	chbx
				ldx	#$77
				lda	#00				;Set at right line
			endif
		endif
		stx	ballx
		sta	ballx+1
		ldy	#00				;Guess + Velocity
		lda	ballvy			;Velocity
		ifmi
			dey					;Prop Sign
		endif
		clc	
		adc	bally				;Add to Position LSB
		tax					;Save LSB
		tya		
		adc	bally+1			;MSB
		ifmi					;Don't let it go off bottom edge
			lda	ttime
			cmp	#$0B
			ifcc
				lda	#snd_b2d
				jsr	dosound
			endif
			lda	#$20			;This rack is over
			sta	brstat
			lda	#00
			tax
		else
			cpx	#$F0			;Off top?
			ifcs				;yes
				lda	#$C0
				ora	brstat		;Save old bits too
				sta	brstat		;Small Paddle
				jsr	chby
				ldx	#$EF
				stx	colflg		;Allow another brick hit
				lda	#00			;Set at the top line
			endif
		endif
		stx	bally
		sta	bally+1
		;Now Draw Ball
		lda	brstat
		and	#$20				;Hold play?
		ifeq
			jsr	vgcntr
			lda	#$40				;X Position
			ldx	#-$4E
			jsr	vgvtr5			;Position to bottom left corner
			ldx	#03
			begin					;Xfer ball position
				lda	ballx,X
				sta	xcomp,X
				dex
			miend
			jsr	vgvtr2		;Position
			lda	#$F7			;White ball
			ldx	#$62
			jsr	vgadd2
			laljsr(mapdot)
			lxhjsr(mapdot)
			jsr	vgadd2
		endif
		
;* Fall through to Brick Collisions....

bline1 	= $b0
bline2 	= $c0
bline3 	= $d0
bline4 	= $e8
paddline 	= $10

		bit	colflg			;Brick collisions allowed??
		ifmi					;yes!
			ldx	#00				;Line 1 to start
			lda	bally				;Look to see if it is at a line
			cmp	#bline1
			ifcs					;At least at line 1
				cmp	#bline2			;And not at line 2
				ifcc					;At line 1
?pdl10				jsr	chklin			;Check for collision
				else					;Above line 2
					inx					;Guess line 2 for now (X=1)
					cmp	#bline3			;Between line 2 and 3?
					bcc	?pdl10			;Check line
					inx					;Guess line 3 now (X=2)
					cmp	#bline4			;At line 4 (Above line 3?)
					bcc	?pdl10			;Yep, check this line
				endif
			endif
		endif
;* Now check paddle *
		lda	ballvy			;Only if ball moving down
		ifmi
			lda	bally
			cmp	#paddline			;At paddle line?
			ifcs					;Above bottom
				cmp	#paddline+$0C
				ifcc
					lda	#$80
					sta	colflg			;Set ok for another hit flag
					jsr	chkpdl
				endif
			endif
		endif
;* Move and output paddle *
		lda	rgdd
		cmp	#$80
		ror	A
		clc	
		adc	paddle
		ifmi
			lda	#00
		endif
		cmp	#$70
		ifcs
			lda	#$70
		endif
		bit	brstat			;Large paddle?
		ifvc					;yes
			cmp	#$68
			ifcs
				lda	#$68
			endif
		endif
		sta	paddle			;Save new paddle position
		clc	
		adc	#04
		sta	xcomp
		lda	#01
		sta	xcomp+1
		lda	#$E0
		sta	xcomp+2
		lda	#$FE
		sta	xcomp+3
		jsr	vgcntr
		jsr	vgvtr2			;Position for paddle
		lda	#$D2
		ldx	#brick7
		jsr	vgadd2			;Color and page select
		laljsr(padlep)			;Paddle pic
		lxhjsr(padlep)		
		bit	brstat			;Check status
		ifvs
			laljsr(brickp)
			lxhjsr(brickp)			;Small paddle
		endif
		jsr	vgadd2			;Draw paddle
		rts
	
;* Paddle Collision Routine *
		
chkpdl	lda	ballx
		sec	
		sbc	paddle			;See if left of left edge
		ifcs					;Right of left edge
			ldx	#00				;Guess small
			bit	brstat			;Large paddle?
			ifvc					;It's large
				inx
			endif
			cmp	middle,X			;Left of middle?
			ifcc					;Is left of middle
				lda	ballvx			;Want moving left here
				ifpl
?cpd10				jsr	chbx				;Get moving left
				endif
?cpd11			jsr	chby
				lda	#snd_b2c
				jsr	dosound
				inc	colcnt
				;Another paddle hit, see if all bricks gone!
				lda	brick
				ora	brick+1
				ora	brick+2			;All Gone?
				ifeq					;yes
					sta	ttime
					ldx	player,abs
					inc	lives,X
					lda	#$50				;5000 points
					jsr	bpont2			;Bonus!!!!
					jsr	dodelay			;Thanks peter!
					lda	#snd_brk
					jsr	dosound			;Make sound too!
					lda	#$80
					sta	bronce
					sta	perm5				;Only thing that perm5 is used for
				endif
			else
				cmp	redge,X			;Past right edge??
				ifcc					;yep
					lda	ballvx			;Want moving right here
					bmi	?cpd10			;Turn it about
					bpl	?cpd11			;Else just change Y
				endif
			endif
		endif
		rts
			
middle	.byte $0A,$0F		;Half Size
redge		.byte $14,$1E		;Size

;* Check for Brick hit in a Line *

chklin	ldy	#-1				;Start at left, brick 0
		lda	ballx				;Check X
		begin
			iny					;Next entry
			cmp	brickx,Y			;Look up in table
		ccend					:Hit this brick (the last one actually!)
		;* Y contains Brick Hit - See if there *
		sty	temp1				;Must have this if needed to turn off brick
		lda	brick,X			;Get brick status
		begin
			rol	A				;Brick status into carry
			dey
		miend
		;* Carry is 1 if brick exists *
		ifcs
			lda	brick,X
			ldy	temp1				;Recall Brick Index
			and	brout,Y			;Drop this bit
			sta	brick,X
			cpx	#02				;Top Line?
			ifeq					;Speed things up!
				lda	brstat			;See if already fast
				and	#$10
				ifeq					;No not fast
					lda	brstat			;Recall old value
					ora	#$10				;Indicate fast
					sta	brstat			;Fast now
					asl	ballvy			;Heheheheheheheheheh
					asl	ballvx
				endif
			endif
			lda	$linscr,X			;Add in brick score
			jsr	brscor
			lda	#$80
			sta	scoflg			;Score change
			lda	#00
			sta	colflg			;Don't allow another hit
			jsr	chby				;Turn the ball about
			lda	#snd_b2b
			jsr	dosound			;Hit a brick
		endif
		rts
		
;* Change the ball velocity *
chbx		lda	ballvx
		jsr	neg					;Change X velocity
		sta	ballvx
		rts
			
chby		lda	ballvy
		jsr	neg					;change Y velocity
		sta	ballvy
		rts
			
add2_b	lda	seqx
		clc	
		adc	#02					;Next Vector (Guess Short)
		sta	seqx
		rts	
		
brscor	ldy	#00					;Guess Player 1
		ldx	player,abs				;Who gets this score
		ifne
			ldy	#score2-score
		endif
		sed	
		clc	
		adc	score,Y
		sta	score,Y
		lda	score+1,Y
		adc	#00
		sta	score+1,Y
		lda	score+2,Y
		adc	#00
		sta	score+2,Y
		lda	score+3,Y
		adc	#00
		sta	score+3,Y
		cld	
		rts
		
;* Add a Life Here! *
addlif	ldx	player,abs
		lda	slives
		clc	
		adc	#01
		sta	lives,X
		lda	#snd_c4
		jmp	dosound
		
;* Tables and Lists *
expdth	.byte 0,0,1,1, 1,1,1,1, 1,1,1,1, 2,2,2,2

;* Message for Warp Hint *
whathint	.byte mwarp1,mwarp2,mwarp3,mwarp4

;* Name Display Color Table *
alcol		.byte $8B,$C6,$CF,$C3,$F1

;* Tact Display Hint Table *
tmesg		.byte mtmes0,mtmes1,mtmes3,mtmes2
tmess 	.byte mcgrn,mcyel,mcred,mcred
linscr	.byte 1,4,8
dislist	.word tact0,tact1,tact3,tact2
hlist		.word scan1,scan2,scan3,scan0

;* Start Intensity For Heart Scan Monitor *
stint		.byte $E0,$C0,$A0,$80,$60,$40,$20

;* Heart Scan Box Color Selection *
hsbox		.byte green,yellow,redr,blue
brout		.byte $7F,$BF,$DF,$EF,$F7,$FB    	;Which bit to drop if brick hit
brickx	.byte $13,$29,$3F,$55,$6B,$80		;Brick X Left Edge
lincol	.byte $D1,$D2,$8B				;Brick Color


;*************************************************
	.sbttl "Space Fort Wave"
;*************************************************
enem2		lda	#05
		sta	hitpts			;500 Per Object
		lda	mzgame
		ifne
			rts
		endif
		lda	saucvl			;Add or subtract some amount
		ldy	saucvd			;Velocity Direction
		ifpl					;Add
			clc	
			adc	#01
			cmp	#$28				;Max Speed
			ifcs
				ldy	#$80
				sty	saucvd			;Start Slowing
			endif
		else					;Else Start Slowing
			sec
			sbc	#02
			ifmi					;Don't let pass 0
				lda	#00
			endif
		endif
		sta	saucvl
		ifeq					;If 0, get a new direction
			sta	saucvd			;And start up again
			ldx	stbix				;Clear base flag index
			bit	rands+2			;Change
			ifmi					;No change bit clear, change ok
				dex					;Guess turn left
				ifvc
					inx
					inx					;Turn Right (2 to compensate for dex)
				endif
			endif
			txa
			and	#07				;Don't let wrap
		else
			lda	stbix				;Else continue same (maybe)
		endif
;*----------------- Limit Check ----------------*
;* Y Check First *
;*----------------------------------------------*
		ldx	stbix				;Which Direction now?
		ldy	statyh			;Check Y motion
		cpy	#02				;If above, get moving down
		ifcc					;It's too high
			ldy	topok,X			;Is it going an ok direction??
			ifpl	
				jsr	stoptime
			else
				ifeq					;Change when stopped
					lda	topchk,X			;Get new direction
					ifmi					;It's straight up, get random direction
						lda	#01				;Either 1 or 7
						bit	rands
						ifmi
							lda	#07
						endif
					endif
				endif
			endif
		endif
		ldy	bonusa			;Bonus 0??
		ifne					;If yes, get off screen
			ldy	statyh			;Must recall position
			cpy	#06
			ifcs						;Too High
				ldy	botok,X				;Going an ok direction
				ifpl						;no
					jsr	stoptime				;Stop it fast
					ifeq						;if stopped, get new direction
						lda	botchk,X				;Bottom check
						ifmi
							lda	#03
							bit	rands+1
							ifmi
								lda	#05					;One Random Check
							endif
						endif
					endif
				endif
			endif
		endif
		;*-------------------------------------------------*
		;*------------ Check X now ------------------------*
		;*-------------------------------------------------*
		ldy	statxh				;X MSB
		cpy	#01					;To far left?
		ifcc
			ldy	ltok,X				;Going an ok direction?
			ifpl						;no
				jsr	stoptime				;Stop it!
				lsr	saucvl
				ifeq							;Stopped?
					lda	sfltchk,X					;Get new left direction
					ifmi							;Straight in, pick new direction
						lda	#05						;Guess RU
						bit	rands
						ifmi
							lda	#07						;Use RD
						endif
					endif
				endif
			endif
		endif
		ldy	statxh				;Reload position
		cpy	#rtedge-1				;To far right?
		ifcs						;yes
			ldy	rtok,X					;Going an ok direction?
			ifpl
				jsr	stoptime
				lsr	saucvl
				ifeq
					lda	sfrtchk,X
					ifmi
						lda	#03					;Guess LU
						bit	rands+1
						ifmi
							lda	#01					;Use LD
						endif
					endif
				endif
			endif
		endif
		sta	stbix
		tax	
		lda	nextmot,X
		ldy	bonusa				;Bonus 0?
		ifeq
			lda	#$80					;Always set down
		endif
		sta	stbflg				;Stop Space Ship
		lda	dropcnt				;Droping from last time?
		ifeq
			lda	frame
			and	#$0F					;Time Interval
			ifeq						;Time for new count
				lda	shtnum,X				;How many shots from here
				sta	dropcnt				;New count
			endif
		else
			ldy	#00
			sty	sobjs2				;Turn off aux stat on 'Fighter' shooting
			jsr	gunloc				;Place 'gun' at station
			lda	#zshot+nmshot-3			;1 shot at first
			ldx	difcty				;Second pass (or better)
			ifne
				lda	#zshot+nmshot-2			;Shoot 2
			endif
			ldx	#00
			stx	temp5					;Flag = from station
			jsr	drop3					;Fire shot
			ifpl						;Started one
				lda	#$11
				sta	objst,Y				;Set to line from here
				dec	dropcnt				;Got one
			endif
		endif
		lda	nenstr
		beq	?enm10
		lda	frame
		tay	
		ifeq
			lda	#$80
			sta	lauen					;Force a start
		endif
		tya	
		and	#$1F
		ifeq
			jsr	emem12
		else
?enm10		jsr	coreg
		endif
		rts	
		
stoptime	ldy	#$80
		sty	saucvd				;Stop it fast
		ldy	saucvl
		rts	
		
;*************************************************
	.sbttl "Gun Location"
;*************************************************
;* Determines the X and Y location of the gun    *
;* based on the current rotation of the base and *
;* which face the gun is on.                     *
;*                                               *
;* Inputs:	temp8,saupic,statx,staty             *
;*          temp8 = face select                  *
;*          a = face (if entry at gunlc2)        *
;*                                               *
;* Output:	xcomp(2) = x posit 			 *
;*   		xcomp+2(2) = same for y              *
;*                                               *
;* Uses:	A,X,temp8                            *
;*                                               *
;* Note: Results are also stored in the locs     *
;*       of the first 3 enemy space ships. The   *
;*       first one of these will be used later   *
;*       so the routines for dropping a shot may *
;*       be used in twenemy. These locs are not  *
;*       used during this space wave so no harm  *
;*       will occur.                             *
;*************************************************
gunloc	lda	statxl
		sta	xcomp				;For output of picture
		sta	sobjxl,Y
		lda	statxh
		sta	xcomp+1
		sta	sobjxh,Y
		lda	statyl
		sta	xcomp+2
		sta	sobjyl,Y
		lda	statyh
		sta	xcomp+3			;xcomp+2 = gun's position y
		sta	sobjyh,Y			;for shot routine later
		rts	

;*************************************************
	.sbttl "Shot Delta"
;*************************************************
;* Calculates the delta X & Y from the gun to    *
;* the ship.                                     *
;*                                               *
;* Inputs:	xcomp   = gun location X             *
;*          xcomp+2 = gun location Y             *
;* (above are not corrected for vanishing point) *
;*   		shipx = ships X position             *
;*          shipy = ships Y position             *
;*                                               *
;* Output:	temp1(2) = delta x                   *
;*          temp2(2) = delta y                   *
;*                                               *
;* Uses:	Y,A,shtdlx,shtdly                    *
;*          2 bytes stack (calls gunloc)         *
;*          temp1,temp2                          *
;*************************************************	
shotdelta	lda	#00
		jsr	gunloc			;Locate gun 0 only
		lda	shipxl			;Locate gun
		sec	
		sbc	xcomp
		sta	temp1				;delta x lsb
		lda	shipxh
		sbc	xcomp+1
		sta	temp1+1			;delta x msb
		sec	
		lda	shipyl
		sbc	xcomp+2
		sta	temp2				;delta y lsb
		lda	shipyh
		sbc	xcomp+3
		sta	temp2+1			;delta y msb
		;* Now scale up the speed for faster shots *
		ldy	#02				;Basic speed
		lda	difcty			;First wave?
		ifne
			iny					;No so go faster
		endif
		tya	
		tax					;Save for below	
		lda	temp1
		begin
			asl	a
			rol	temp1+1
			dey
		miend
		sta	temp1
		lda	temp2
		begin
			asl	a
			rol	temp2+1
			dex
		miend
		sta	temp2
		rts	
		
;********************************************
	.sbttl "Motion Limits Table"
;********************************************
nextmot	.byte $80,$A0,$20,$E0,$C0,$F0,$30,$B0

topchk	.byte $00,$01,$02,$01,$80,$07,$06,$07	;Top check alt correctio table

botchk	.byte $80,$03,$02,$03,$04,$05,$06,$05	;Bottom check alt correction table

sfltchk		.byte $00,$07,$80,$05,$04,$05,$06,$07	;Left edge check alt correction table

sfrtchk		.byte $00,$01,$02,$03,$04,$03,$80,$01	;Right edge table

;Direction Okay truth table
topok		.byte $80,$80,$80,$00,$00,$00,$80,$80	;80 = direction ok
botok		.byte $00,$00,$80,$80,$80,$80,$80,$00
ltok		.byte $80,$00,$00,$00,$80,$80,$80,$80
rtok		.byte $80,$80,$80,$80,$80,$00,$00,$00

;These are D,DL,L,UL,U,UR,R,DR
shtnum		.byte $01,$03,$03,$02,$01,$02,$01,$03


;*********************************************
	.title "Message Routine"
	.sbttl "Message Data"
;*********************************************
;* Message Macro                             *
;* Will build an absolute address table here *
;* of each message. Space for table has to be*
;* initially declared adequately. The macro  *
;* will also define a new label for each     *
;* message that can be used to get the       *
;* proper index into the message table.      *
;*********************************************
___msnum = 0
___nmsgs = 95d
___eng = $
___csy = ___eng+(___nmsgs*2)
engmsg = ___eng
msglbs = ___csy

#define 	mess(xlit,xcol,xsca,xypo)  \ .org ___eng
#defcont   \ .word xlit
#defcont   \___eng .set ___eng+2
#defcont   \m+xlit = ___msnum
#defcont   \___msnum .set ___msnum+2
#defcont   \ .org ___csy
#defcont   \ .byte ((xcol*$10)|(xsca))
#defcont   \ .byte xypo
#defcont   \___csy .set ___csy+2

;******************************************************************************
	.sbttl "Messages"
;******************************************************************************
;* Data: Message Pointer, Color, Scale, Y Position                            *
;******************************************************************************
		
		mess(gamov,colyellow,1,$24)		;Game Over
		mess(press,colred,1,$24)		;Press Start
		mess(playr,colyellow,1,$1a)		;Player (Big)
		mess(addm1,colcyan,1,4)			;Add Time Message
		mess(addm2,colcyan,1,-6)		;Rest of Message
		mess(oxyg,colgreen,1,10d)		;Oxygen
		mess(reac,colred,1,-30d)		;Reactor
		mess(bonus,colgreen,1,$54)		;Bonus
		mess(time,colorange,1,$98)		;Time
		mess(tmes0,colcyan,1,$48)		;Destroy...
		mess(tmes1,colcyan,1,$48)		;Shoot all ...
		mess(tmes2,colcyan,1,$48)		;Hit...
		mess(tmes3,colcyan,1,$48)		;Maneuver through maze
		mess(hint0,colred,1,$46)		;Dock...
		mess(mzh0,colcyan,1,$48)		;Maze Hint for Maze 0
		mess(mzh1,colcyan,1,$50)		;Additional for Maze 0
		mess(mzh2,colcyan,1,$50)		;For Maze 1
		mess(mzh3,colcyan,1,$50)		;For Maze 2
		mess(mzh4,colcyan,1,$50)		;For Maze 3
		mess(mzh5,colcyan,1,$50)		;For Maze 4
		mess(mzh6,colcyan,1,$50)		;For Maze 5
		mess(mzh7,colcyan,1,$50)		;For Maze 6
		mess(mzh8,colcyan,1,$50)		;For Maze 7
		mess(mzh9,colcyan,1,$50)		;For Maze 8
		mess(mzha,colcyan,1,$50)		;For Maze 9
		mess(mzhb,colcyan,1,$50)		;For Maze 10
		mess(mzhc,colcyan,1,$50)		;For Maze 11
		mess(mzhd,colcyan,1,$50)		;For Maze 12
		mess(mlevel,colyellow,1,-$50)		;Level
		mess(insert,colred,1,$24)		;Insert Coins
		mess(atari,colbluer,1,$98)		;MCMLXXXIII Atari
		mess(atar2,colbluer,1,$24)		;MCMLXXXIII Atari (Different Position)
		mess(credi,colcyan,1,$18)		;Credits
		mess(bolif,colcyan,1,$a0)		;Bonus Every
		mess(elife,colpurple,1,$a8)		;Lives
		mess(cont,colwhite,1,$60)		;Condition
		mess(mred,colredr,1,$a2)		;Red (For Warp Description)
		mess(myel,colyellow,1,$a2)		;Yellow 
		mess(mgrn,colgreen,1,$a2)		;Green
		mess(maqu,colcyan,1,$a2)		;Aqua
		mess(cred,colred,1,$60)			;Red
		mess(cyel,colyellow,1,$60)		;Yellow
		mess(cgrn,colgreen,1,$60)		;Green
		mess(garbage,colred,1,$54)		;Garbage Ejected
		mess(conf,colwhite,2,$38)		;Confirmed
		mess(mdist,colcyan,2,-$38)		;Range
		mess(etyp,colcyan,2,-$3e)		;Enemy Type
		mess(cols,colcyan,2,-$38)		;Closing
		mess(hold,colcyan,2,-$38)		;Holding
		mess(sphr,colcyan,2,-$3e)		;Spheroids
		mess(figh,colcyan,2,-$3e)		;Fighters
		mess(star,colcyan,2,-$3e)		;Starcastle
		mess(spin,colcyan,2,-$3e)		;Spinners
		mess(warp0,colgreenr,1,$54)		;Intercepted Message
		mess(warp1,colgreenr,1,$48)		;Enter 21 for...
		mess(warp2,colgreenr,1,$48)		;Enter 42 for...
		mess(warp3,colgreenr,1,$48)		;Enter 824 for...
		mess(warp4,colgreenr,1,$48)		;Enter 315 for...
		mess(his,colred,1,$48)			;High Score
		mess(gtsc,colgreenr,0,$24)		;Great Score
		mess(enin,colyellow,1,$10)		;Enter your initials
		mess(getout,colredr,0,0)		;Get Out!!
		mess(pub0,colwhite,0,0)			;Push Button
		mess(pub1,colwhite,1,0)			;For Higher Jumps
		mess(act4,colwhite,1,$18)		;Accounting Message
		mess(act4a,colwhite,1,$20)		;Accounting Message
		mess(act5,colwhite,1,$28)		;Accounting Message
		mess(act7,colwhite,1,-$40)		;Accounting Message
		mess(act8,colwhite,1,-$40)		;Accounting Message
		mess(act9,colwhite,1,-$40)		;Accounting Message
		mess(acta,colwhite,1,-$40)		;Accounting Message
		mess(actb,colwhite,1,-$40)		;Accounting Message
		mess(actc,colred,1,-$30)		;Accounting Message
		mess(actd,colred,1,-$30)		;Accounting Message
		mess(acte,colred,1,-$30)		;Accounting Message
		mess(actf,colred,1,-$30)		;Accounting Message
		mess(ac10,colyellow,1,$3d)
		mess(ac11,colyellow,1,$3d)
		mess(ac12,colorange,1,$21)	
		mess(ac13,colorange,1,$21)
		mess(cmode,colgreen,1,$2e)		;Free Play
		mess(cmod1,colgreen,1,$2e)		;1 coin 2 plays
		mess(cmod2,colgreen,1,$2e)		;1 coin 1 play
		mess(cmod3,colgreen,1,$2e)		;2 coins 1 play
		mess(cmod4,colorange,1,$36)		;2 coin minimum
		
gamov		.byte $E0,$22,$16,$2E,$1E,$00,$32,$40,$1E,$B8
		
press		.byte $D8,$34,$38,$1E,$3A,$3A,$00,$3A,$3C,$16,$38,$BC
		
addm2		.byte $CA,$3C,$32,$00,$3A,$3C,$16,$38,$3C,$00,$16,$3C,$00,$2C,$1E,$40,$1E,$2C,$80
		
addm1		.byte $A9,$34,$38,$1E,$3A,$3A,$00,$3A,$3C,$16,$38,$3C,$00,$42,$26
		.byte $3C,$24,$26,$30,$00,$00,$00,$00,$3A,$1E,$1A,$32,$30,$1C,$BA
		
playr		.byte $E0,$34,$2C,$16,$46,$1E,$38,$80
		
tmes0		.byte $C7,$1C,$1E,$3A,$3C,$38,$32,$46,$00,$20,$26,$3A,$24,$00,$38,$32,$18,$32,$3C,$BA
		
tmes1		.byte $C7,$1C,$1E,$3A,$3C,$38,$32,$46,$00,$1E,$30,$1E,$2E,$46,$00,$3A,$24,$26,$34,$BA
		
tmes2		.byte $A3,$3A,$24,$32,$32,$3C,$00,$20,$26,$22,$24,$3C,$1E,$38,$3A,$52,$00
		.byte $16,$40,$32,$26,$1C,$00,$38,$1E,$1C,$00,$2C,$26,$30,$1E,$BA
		
tmes3		.byte $C1,$2E,$16,$30,$1E,$3E,$40,$1E,$38,$00,$3C,$24,$38,$32,$3E,$22,$24
		.byte $00,$2E,$16,$48,$9E
		
hint0		.byte $BE,$1C,$32,$1A,$2A,$00,$32,$30,$00,$42,$24,$26,$3C,$1E,$00,$34,$2C,$16,$3C,$20,$32,$38,$AE

mzh0		.byte $A3,$1E,$44,$26,$3C,$00,$2E,$16,$48,$1E,$00,$18,$1E,$20,$32,$38,$1E,$00,$3C,$26
		.byte $2E,$1E,$38,$3A,$00,$38,$1E,$16,$1A,$24,$00,$82
		
mzh1		.byte $A3,$20,$32,$2C,$2C,$32,$42,$00,$16,$38,$38,$32,$42,$3A,$00,$16,$30,$1C,$00,$3C
		.byte $32,$3E,$1A,$24,$00,$38,$1E,$16,$1A,$3C,$32,$B8
		
mzh2		.byte $A9,$34,$26,$1A,$2A,$00,$3E,$34,$00,$1C,$32,$3C,$3A,$00,$20,$32,$38,$00,$1E,$44
		.byte $3C,$38,$16,$00,$32,$44,$46,$22,$1E,$B0
		
mzh3		.byte $80,$3E,$3A,$1E,$00,$3A,$24,$26,$1E,$2C,$1C,$3A,$00,$3C,$32,$00,$1C,$1E,$3A,$3C,$38,$32
		.byte $46,$00,$04,$00,$38,$32,$18,$32,$3C,$00,$32,$38,$00,$20,$26,$38
		.byte $1E,$18,$16,$2C,$AC
		
mzh4		.byte $9D,$28,$3E,$2E,$34,$00,$20,$26,$38,$1E,$18,$16,$2C,$2C,$3A,$00
		.byte $3C,$32,$00,$16,$40,$32,$26,$1C,$00,$1A,$32,$2C,$2C,$26,$3A,$26
		.byte $32,$B0
		
mzh5		.byte $91,$38,$1E,$1C,$00,$3C,$38,$26,$34,$00,$34,$16,$1C,$3A,$00,$2C
		.byte $16,$3E,$30,$1A,$24,$00,$1C,$1E,$16,$1C,$2C,$46,$00,$20,$26,$38
		.byte $1E,$18,$16,$2C,$2C,$BA
		
mzh6		.byte $9A,$3A,$24,$26,$1E,$2C,$1C,$00,$42,$26,$2C,$2C,$00,$16,$18,$3A
		.byte $32,$38,$18,$00,$0A,$00,$38,$32,$18,$32,$3C,$00,$18,$3E,$2C,$2C
		.byte $1E,$3C,$BA
		
mzh7		.byte $97,$26,$00,$1A,$16,$30,$3C,$00,$18,$1E,$2C,$26,$1E,$40,$1E,$00
		.byte $46,$32,$3E,$00,$2E,$16,$1C,$1E,$00,$26,$3C,$00,$3C,$24,$26,$3A
		.byte $00,$20,$16,$B8
		
mzh8		.byte $8E,$26,$20,$00,$46,$32,$3E,$00,$16,$38,$1E,$00,$3A,$32,$00,$22
		.byte $32,$32,$1C,$52,$00,$46,$32,$3E,$00,$20,$26,$30,$1C,$00,$3C,$24
		.byte $1E,$00,$34,$16,$3C,$24,$CE
		
mzh9		.byte $8B,$42,$16,$3C,$1A,$24,$00,$32,$3E,$3C,$00,$20,$32,$38,$00,$1A
		.byte $24,$16,$30,$22,$26,$30,$22,$00,$42,$16,$2C,$2C,$3A,$00,$16,$30
		.byte $1C,$00,$20,$2C,$32,$32,$38,$BA
		
mzha		.byte $8E,$24,$26,$30,$3C,$00,$50,$00,$3C,$24,$1E,$00,$34,$16,$3C,$24
		.byte $00,$26,$30,$00,$26,$3A,$00,$30,$32,$3C,$00,$3C,$24,$1E,$00,$34
		.byte $16,$3C,$24,$00,$32,$3E,$BC
		
mzhb		.byte $8E,$20,$32,$2C,$2C,$32,$42,$00,$3C,$24,$1E,$00,$34,$16,$3C,$24
		.byte $00,$3C,$24,$38,$32,$3E,$22,$24,$00,$1A,$24,$16,$30,$22,$26,$30
		.byte $22,$00,$42,$16,$2C,$2C,$BA
		
mzhc		.byte $94,$2A,$1E,$1E,$34,$00,$34,$2C,$16,$46,$26,$30,$22,$52,$00,$3C
		.byte $24,$1E,$00,$24,$32,$2E,$1E,$00,$42,$32,$38,$2C,$1C,$00,$26,$3A
		.byte $00,$30,$1E,$16,$B8
		
mzhd		.byte $9A,$2C,$16,$3A,$1E,$38,$00,$1A,$16,$30,$30,$32,$30,$00,$3A,$3E
		.byte $34,$34,$32,$38,$3C,$00,$26,$3A,$00,$30,$32,$3C,$00,$1C,$1E,$16
		.byte $1C,$2C,$C6
		
oxyg		.byte $EB,$32,$44,$46,$22,$1E,$30,$80

reac		.byte $AA,$38,$1E,$16,$1A,$3C,$32,$B8

bonus		.byte $2F,$18,$32,$30,$3E,$3A,$80
		
time		.byte $E8,$3C,$26,$2E,$9E

mlevel	.byte $9E,$2C,$1E,$40,$1E,$2C,$80

insert	.byte $D4,$26,$30,$3A,$1E,$38,$3C,$00,$1A,$32,$26,$30,$BA

atari
atar2		.byte $C5,$00,$7E,$2E,$1A,$2E,$2C,$44,$44,$44,$26,$26,$26,$00,$16,$3C,$16,$38,$A6

credi		.byte $D4,$1A,$38,$1E,$1C,$26,$3C,$3A,$80

bolif		.byte $C8,$18,$32,$30,$3E,$3A,$00,$1E,$40,$1E,$38,$46,$80

elife		.byte $CD,$00,$2C,$26,$40,$1E,$3A,$00,$34,$1E,$38,$00,$34,$2C,$16,$46,$1E,$B8

cont		.byte $D0,$1A,$32,$30,$1C,$26,$3C,$26,$32,$30,$80

cred		.byte $10,$38,$1E,$9C

cyel		.byte $10,$46,$1E,$2C,$2C,$32,$C2

cgrn		.byte $10,$22,$38,$1E,$1E,$B0

garbage	.byte $98,$22,$16,$38,$18,$16,$22,$1E,$00,$1E,$28,$1E,$1A,$3C,$1E,$9C

conf		.byte $42,$1A,$32,$30,$20,$26,$38,$2E,$1E,$9C

mdist		.byte $D4,$38,$16,$30,$22,$1E,$80

etyp		.byte $D4,$1E,$30,$1E,$2E,$46,$00,$3C,$46,$34,$1E,$80

cols		.byte $04,$1A,$2C,$32,$3A,$26,$30,$A2

hold		.byte $04,$24,$32,$2C,$1C,$26,$30,$A2

figh		.byte $04,$20,$26,$22,$24,$3C,$1E,$38,$BA

sphr		.byte $04,$3A,$34,$24,$1E,$38,$32,$26,$1C,$BA

star		.byte $04,$3A,$34,$16,$1A,$1E,$00,$20,$32,$38,$BC

spin		.byte $04,$3A,$34,$26,$30,$30,$1E,$38,$BA

warp0		.byte $B5,$50,$50,$50,$26,$30,$3C,$1E,$38,$1A,$1E,$34,$3C,$1E,$1C,$00,$2E,$1E,$3A,$3A,$16,$22,$1E,$50,$50,$D0

warp1		.byte $A0,$1E,$30,$3C,$1E,$38,$00,$06,$08,$00,$20,$32,$38,$00,$38,$1E,$1C
		.byte $00,$42,$16,$38,$34,$00,$3C,$32,$00,$2C,$1E,$40,$1E,$2C,$00,$8A
		
warp2		.byte $96,$3E,$3A,$1E,$00,$0A,$0E,$00,$20,$32,$38,$00,$46,$1E,$2C,$2C,$32
		.byte $42,$00,$42,$16,$38,$34,$00,$3C,$32,$00,$2C,$1E,$40,$1E,$2C,$00,$94
		
warp3		.byte $96,$3E,$3A,$1E,$00,$12,$06,$0A,$00,$20,$32,$38,$00,$22,$38,$1E,$1E
		.byte $30,$00,$42,$16,$38,$34,$00,$3C,$32,$00,$2C,$1E,$40,$1E,$2C,$00,$04,$82
		
warp4		.byte $96,$3E,$3A,$1E,$00,$08,$04,$0C,$00,$20,$32,$38,$00,$16,$36,$3E,$16
		.byte $00,$42,$16,$38,$34,$00,$3C,$32,$00,$2C,$1E,$40,$1E,$2C,$00,$04,$88
		
his		.byte $DF,$24,$26,$22,$24,$00,$3A,$1A,$32,$38,$1E,$BA

enin		.byte $C7,$1E,$30,$3C,$1E,$38,$00,$46,$32,$3E,$38,$00,$26,$30,$26,$3C,$26,$16,$2C,$BA
		
getout	.byte $EB,$22,$1E,$3C,$00,$32,$3E,$BC

pub0		.byte $DC,$24,$32,$2C,$1C,$00,$00,$18,$3E,$3C,$3C,$32,$B0

pub1		.byte $D0,$20,$32,$38,$00,$24,$26,$22,$24,$1E,$38,$00,$28,$3E,$2E,$34,$BA

gtsc		.byte $C0,$22,$38,$1E,$16,$3C,$00,$3A,$1A,$32,$38,$9E

act4		.byte $90,$04,$00,$34,$2C,$16,$46,$1E,$38,$00,$22,$16,$2E,$1E,$3A,$80

act4a		.byte $90,$06,$00,$34,$2C,$16,$46,$1E,$38,$00,$22,$16,$2E,$1E,$3A,$80

act5		.byte $C0,$16,$40,$22,$00,$22,$16,$2E,$1E,$00,$3C,$26,$2E,$1E,$80

act7		.byte $CA,$38,$1E,$3A,$1E,$3C,$00,$24,$26,$22,$24,$00,$3A,$1A,$32,$38,$1E,$3A,$80

act8		.byte $DC,$38,$1E,$3A,$1E,$3C,$00,$3C,$26,$2E,$1E,$3A,$80

act9		.byte $D2,$1A,$32,$26,$30,$00,$1A,$32,$3E,$30,$3C,$1E,$38,$80

acta		.byte $DC,$3A,$32,$3E,$30,$1C,$3A,$00,$32,$20,$A0

actb		.byte $E6,$3A,$32,$3E,$30,$1C,$80

actc		.byte $F4,$1C,$1E,$2E,$B2

actd		.byte $F4,$1E,$16,$3A,$C6

acte		.byte $EE,$2E,$1E,$1C,$26,$3E,$AE

actf		.byte $F4,$24,$16,$38,$9C

ac10		.byte $E8,$16,$1C,$16,$34,$3C,$00,$32,$B0

ac11		.byte $E2,$16,$1C,$1C,$16,$34,$3C,$00,$32,$20,$A0

ac12		.byte $DF,$16,$3C,$3C,$00,$3A,$30,$1C,$3A,$00,$32,$B0

ac13		.byte $DC,$16,$3C,$3C,$00,$3A,$30,$1C,$3A,$00,$32,$20,$A0

cmode		.byte $D4,$00,$00,$20,$38,$1E,$1E,$00,$34,$2C,$16,$C6

cmod1		.byte $D4,$04,$00,$1A,$32,$26,$30,$00,$06,$00,$34,$2C,$16,$46,$BA

cmod2		.byte $D4,$04,$00,$1A,$32,$26,$30,$00,$04,$00,$34,$2C,$16,$C6

cmod3		.byte $D4,$06,$00,$1A,$32,$26,$30,$3A,$00,$04,$00,$34,$2C,$16,$C6

cmod4		.byte $CA,$06,$00,$1A,$38,$1E,$1C,$26,$3C,$00,$2E,$26,$30,$26,$2E,$3E,$AE

mred		.byte $C4,$1E,$30,$3C,$1E,$38,$00,$38,$1E,$1C,$00,$42,$16,$38,$34,$50,$50,$50,$50,$50,$80

myel		.byte $C4,$1E,$30,$3C,$1E,$38,$00,$46,$1E,$2C,$2C,$32,$42,$00,$42,$16,$38,$34,$50,$50,$80

mgrn		.byte $C4,$1E,$30,$3C,$1E,$38,$00,$22,$38,$1E,$1E,$30,$00,$42,$16,$38,$34,$50,$50,$50,$80

maqu		.byte $C7,$1E,$30,$3C,$1E,$38,$00,$16,$36,$3E,$16,$00,$42,$16,$38,$34,$50,$50,$50,$80

;*******************************************************
	.sbttl "Main Message Routine"
;*******************************************************
;* Will output specified message to specified location *
;* on screen.                                          *
;*                                                     *
;* Inputs: X = message # * 2                           *
;*******************************************************
mesg		lda	msglbs+1,X
msgen3	stx	temp4
		sta	temp2
		ldy	temp4
		lda	(litral,Y)				;Get Literal ptr
		sta	xcomp
		iny	
		lda	(litral,Y)
		sta	xcomp+1
		ldy	#00
		lda	(xcomp,Y)				;Get horiz position from literal
		sta	temp1
msgent	jsr	vgcntr
msgen2	lda	#00
		sta	vgbrit
		lda	#02
		jsr	vgsca1
		lda	temp1
		ldx	temp2
		jsr	vgvtr1				;Position Beam (Use vgbrit)
msgnop	ldy	temp4					;Set up ptr to literal
		lda	(litral,Y)
		sta	xcomp
		iny	
		lda	(litral,Y)
		sta	xcomp+1
		ldx	temp4
		lda	msglbs,X
		pha	
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		ora	#$F0
		tay	
		lda	#00
		jsr	vgstat				;Set color
		pla	
		and	#$0F
		clc	
		adc	#01
		ldy	#$30
		jsr	vgscal				;Set scale
		ldy	#01
		lda	#00					;Init vglist offset
		sta	temp1
		begin
			lda	(xcomp,Y)				;Get character representation
			sta	temp2
			and	#$7F
			iny	
			sty	temp3
			tax	
			lda	vgmsga,X				;Get correct JSRL
			ldy	temp1
			sta	(vglist,Y)
			iny	
			lda	vgmsga+1,X
			sta	(vglist,Y)
			iny	
			sty	temp1					;Save y
			ldy	temp3					;Get character ptr
			bit	temp2					;if not end of string
		miend
		ldy	temp1
		dey	
		jmp	vgadd
		
;* No vertical positioning *
msgfol	stx	temp4				;Input: x = msg#*2
		sta	temp1				;	  A = x offset
		lda	#00
		sta	temp2
		beq	msgen2			;Always
		
;**********************************************
	.sbttl "Header Time Info"
;**********************************************
header	bit	mzgame,abs
		ifmi					;We are in maze
			lda	mtim				;Doing hint?
			ifeq					;no, this is okay
				jsr	vgcntr
				lda	#00
				ldx	#$71
				jsr	vgadd2
				lda	#$12
				ldx	#$2B
				jsr	vgvtr5
				ldx	#moxyg
				jsr	msgfak
				jsr	vgcntr
				lda	#00
				ldx	#$71
				jsr	vgadd2
				lda	#$31
				ldx	#$2B
				jsr	vgvtr5
				ldx	#mreac
				jsr	msgfak
			endif
		endif
		rts
			
msgfak	stx	temp4
		jmp	msgnop
		
chksum6	.byte $40

ininitls	.byte 0,0,0			;EEROM will overwrite
		.byte $19,$1C,$1C
		.byte $1D,$1C,$0D
		.byte $0E,$0F,$1D
		.byte $14,$17,$1C
		.byte $16,$20,$1C
		
;*****************************************************
	.title "Self Test Routines"
	.sbttl "Reset Memory"
;*****************************************************

testnm	=	$A0		;Need a byte here
pnttbl	=	$E0		;11 checksums
erplc		=	$F0		;RAM and such error regs
gamerr	=	$F9		;Gamma Errors
nxtsnd	=	$FA		;Next sound index

selftest	sei	
		ldx	#$FF
		txs	
		sta	vgreset
		cld	
		lda	#$20
		sta	plysel
		lda	#$2C
		sta	plysel
		lda	#00
		tax	
		begin
			sta	$0000,X
			sta	$0100,X
			sta	$0200,X
			sta	$0300,X
			sta	$0400,X
			sta	$0500,X
			sta	$0600,X
			sta	$0700,X
			sta	$0800,X
			sta	$0900,X
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
			sta	comram,X
			sta	comram+$100,X
			sta	comram+$200,X
			sta	comram+$300,X
			sta	comram+$400,X
			sta	comram+$500,X
			sta	comram+$600,X
			sta	comram+$700,X
			sta	comram+$800,X
			sta	comram+$900,X
			sta	comram+$A00,X
			sta	comram+$B00,X
			sta	comram+$C00,X
			sta	comram+$D00,X
			sta	comram+$E00,X
			sta	comram+$F00,X
			sta	watchdog
			ldy	#01
			sty	rampg				;Clear alt RAM page too!
			sta	$0300,X
			sta	$0400,X
			sta	$0500,X
			sta	$0600,X
			sta	$0700,X
			sta	rampg				;Set page back
			inx	
		eqend
		lda	#plrslb+$0c			;Need to select self test switch
		sta	inverts
		lda	ststsw			;If self test
		and	#ststbit
		ifne					;no
			jmp	pwron			;Goto Game Reset
		endif

;********************************************
	.sbttl "Zero Page Test"
;********************************************
stest0	lda	#$FF
		ldx	#00
		begin
			sta	stroyl,X			;First set RAM to $FF
			dex
		eqend
?sts20	lda	$0900,X
		eor	#$FF
		bne	?sts33			;Got a bad RAM here
		lda	#01				;RAM is 2K x 8, use 1,2,4,8,10,20,40,80 test
?sts30	sta	$0900,X			;Write out test pattern
		tay	
		eor	$0900,X
		bne	?sts33			;Not the same, oh shit! An error!
		tya	
		beq	?sts35			;Stop when RAM is 0
		eor	#$FF				;Write more than one bit
		sta	$0900,X			;Ed says this causes some RAM's to fail
		eor	$0900,X
		bne	?sts33			;Get an Error?
		tya	
		asl	a
		jmp	?sts30
?sts33	ldy	#01
		bne	rambad			;Report top RAM bad!
		;* If this RAM bad, watchdog will loop and keep trying for us! *
?sts35	inx	
		bne	?sts20			;Next location
		stx	rands				;Test 0 page
		lda	#07
		jmp	tst2k				;Now test 0 to 7ff
		;* Above checks 0,1 and swap 0 of swapping RAM 200-7ff and 800
zpgtst	ifcs					;Return here from test routine
			ldy	#00				;Report bad low RAM
			beq	rambad			;Go and die!!
		endif
		lda	#(vecram&$ff00)/$100	;Do vector RAM
		sta	tstart
		clc	
		adc	#07				;Test first 2K
		jsr	tst2k
		ldy	#02
		ifcs
			tya	
			sta	erplc,Y			;Bad RAM place
			ldx	#snd_c4			;and beep here!
			jmp	die				;This one bad
		endif
		lda	#((vecram+$800)&$ff00)/$100
		sta	tstart
		clc	
		adc	#07
		jsr	tst2k
		ldy	#03				;Report low vec RAM
		ifcs
			jsr	rambad			;Report and return (if possible)
		endif
		lda	#(comram&$ff00)/$100	;Alpha/Beta Communication RAM
		sta	tstart
		clc	
		adc	#07
		jsr	tst2k				;Test 2K com RAM
		ldy	#04				;Error for bad com RAM
		ifcs
			jsr	rambad
		endif
		lda	#01
		sta	rampg			;Swap to RAM page 1
		lda	#02
		sta	tstart		;Do 200-8ff
		lda	#08
		jsr	tst2k			;Test swap RAM
		ldx	#00
		stx	rampg			;Reset to 0
		ldy	#01			;This is also swap RAM
		ifcs
			jsr	rambad
		endif
		jmp	stest3		;RAM Ok, do ROM tests now
		
		
;********************************************
;* We have an error, so how do we tell the  *
;* real world???                            *
;*     y = section that failed              *
;********************************************
rambad	tya	
		sta	erplc,Y		;Save possible failures
		tya				;See if this was page 0
		ifne
			rts
		endif
		ldx	#snd_c7		;Beep alot
die		lda	#xmigama
		begin
			sta	watchdog
			bit	portstat		;Wait for Gamma
		neend
		lda	portrd		;Get data
		beq	die			;This is first response
		stx	portwr		;Send the sound
?sts10	lda	ststsw		;Wait for switch to turn off
		and	#ststbit
		sta	watchdog
		beq	?sts10
?sts15	bne	?sts15		;Now spin and die

;*********************************************
	.sbttl "Checksum Utilities"
;*********************************************
;* dosum - used to add check sum of a ROM.   *
;*                                           *
;* Entry:	temp1 = ptr to ROM to csum       *
;*          x     = number of pages to use   *
;*          temp3 = which checksum           *
;*                                           *
;* Exit:	temp3 updated                    *
;*  		x,pnttbl checksum stored here    *
;*********************************************
dosum		stx	temp2			;Save page count
		lda	temp3			;Use number as seed
		begin
			begin	
				eor	(temp1,Y)
				iny
			eqend
			inc	temp1+1
			sta	watchdog		;No Watchdog
			dec	temp2
		eqend
		ldx	temp3
		sta	pnttbl,X		;Checksums
		inc	temp3			;Indicated 1 more
		rts	
		
;*********************************************
;* setpgv - set vector page for ROM checksum *
;*          this sets the page, executes a   *
;*          vggo and then waits for the halt *
;*          flag. At that point the proper   *
;*          page should be selected.         *
;*                                           *
;* Entry:	A = page to select               *
;*********************************************		
setpgv	sta	vgreset		;Make sure stopped
		ora	#$60
		sta	vecram+1
		sta	vecram		;Stat instruction
		lda	#$20
		sta	vecram+2
		sta	vecram+3		;Halt instruction
		sta	vggo			;Start VG
		nop
		nop
		nop
		nop
		lda	#haltbit		;Look for halt
		begin
			bit	halt			;Stopped?
		neend
		rts
		
;************************************************
	.sbttl "ROM Test"
;************************************************
;* The following is a table of ROM's which must *
;* be check-sumed, their "real" address, and    *
;* any paging if necessary.                     *
;*                                              *
;*  Address            Pages        Paging      *
;* 5000-5FFF		16.		No          *
;* 6000-7FFF 		32.		VCTR 0      *
;* 6000-7FFF 		32.		VCTR 1      *
;* 6000-7FFF 		32.		VCTR 2      *
;* 6000-7FFF 		32.		VCTR 3      *
;* 8000-BFFF		64.		No		*
;* C000-FFFF		64.		No 		*
;* 2000-3FFF		32.		PRGM 0      *
;* 2000-3FFF		32.		PRGM 1      *
;* 2000-3FFF		32.		PRGM 2      *
;* 2000-3FFF		32.		PRGM 3      *
;*                                              *
;* To set VCTR paging, a small program must be  *
;* transferred to the vector RAM which does a   *
;* STAT and PAGE instruction then halts. This   *
;* is the only way to assure that the proper    *
;* vector page is selected. The vector gen      *
;* must not be running during the test.         *
;* To set program page, a store to global addr  *
;* ROMPG must be done with the desired page #   *
;************************************************
stest3	lda	#00
		tay	
		tax	
		sta	temp1				;Bottom is always 0
		lda	#(vecrom/$100)		;Start at 5000
		sta	temp1+1
		txa					;Seed for cksum
		sta	temp3				;Save this
		ldx	#16d				;Number of pages
		jsr	dosum				;5000-5FFF
		tax					;Did this come back 0??
		ifne					;To bad, bad vec main page
			ldx	#snd_d5
			jmp	die
		endif
		lda	#00
		sta	temp4				;Which page
		begin
			lda	temp4				;Set vector page
			jsr	setpgv			;Set vector page
			lda	#(vecrom+$1000)/$100	;All pages start at same place
			sta	temp1+1
			ldx	#32d
			jsr	dosum				;This will bump temp3 for us
			inc	temp4				;next??
			lda	temp4
			cmp	#04				;Done all 4?
		eqend
		lda	#(program/$100)		;Do main page program now
		sta	temp1+1
		ldx	#64d				;Lots of pages here
		jsr	dosum
		lda	#(program+$4000)/$100	
		sta	temp1+1
		ldx	#64d
		jsr	dosum				;Do C000-FFFF
		lda	#00
		sta	temp4				;Now do programming paging
		begin
			lda	temp4
			sta	rompg
			lda	#($2000/$100)		;All pages start at the same place
			sta	temp1+1			;do
			ldx	#32d				;Do 32 pages
			jsr	dosum				;This will bump temp3 for us
			inc	temp4				;Next?
			lda	temp4
			cmp	#04				;Done all 4?
		eqend

;*****************************************
;* End of ROM tests                      *
;*                                       *
;* (temp2,+1)   = scratch                *
;* (pnttbl,+10d = 11 checksums for ROM's *
;* (temp3)      = diag step #            *
;*****************************************
		lda	#xmigama			;Has gamma transmitted?	
		ldx	#00			
		sta	watchdog
		stx	gamerr			;Guess Gamma bad (0 is bad)
		stx	nxtsnd			;Start with sound index 0
		begin
			dex					;Try 255 times to get gamma response
			beq	gammabad			;Gamma is not working
			bit	portstat			;Gamma should return quickly
		neend					;Should respond
gammabad	lda	portrd			;Get the Data
		;* First response ofter a reset is -1 *
		sta	gamerr			;Gamma should return off
		;* Anything else coming back is an error *
		jsr	initcol			;Init Color RAM
		jsr	inilt2			;Read in Options now
		jmp	diagm				;Do Diag Main Line
		
;*******************************************
	.sbttl "Gamma Test"
;*******************************************
stest4	lda	gamerr			;See if Gamma was bad
		ifpl					;Bad Gamma, a Big red G please!
			jsr	vgcntr			
			lda	#$F4
			ldx	#$60
			jsr	vgadd2			;Add color
			lda   vgmsga+$22			;G's JSRL location
			ldx	vgmsga+$23
			jmp	vgadd2
		endif
mameskip	cmp	#-1				;Any Errors
		ifne
			;Errors:  Bottom Bits represent errors if 0
			;	
			;	D0 = Gamma RAM Error
			;	D1 = Gamma ROM Error
			;	D2 = Gamma Pokey Error
			; 	D3 = Gamma EEROM Error
			
			sta	temp2			;Save Errors
			jsr	vgcntr
			lda	#-10d
			ldx	#$50
			jsr	vgvtr5
			ldx	#03
			stx	temp4
			begin
				ldx	temp4
				ldy	#00			;Guess Blank (ok)
				lsr	temp2			;Check bit
				ifcc				;Got an error
					ldy	gamnws,X		;Get error letter
				endif
				lda	vgmsga,Y
				ldx	vgmsga+1,Y		;Display a letter
				jsr	vgadd2
				dec	temp4			;Any more??
			miend
		endif
		jsr	vgcntr			;This to change later (no RAM if possible)
		lda	#-$20
		ldx	#$10
		jsr	vgvtr5
		lda	#02				;Scale 2
		jsr	vgsca1			;Big numbers
		lda	#07
		sta	temp2
		jsr	getoptn			;Option Switches
		jsr	dis01				;Display
		lda	#-$3F
		ldx	#-$0A
		jsr	vgvtr5			;Position for next line
		lda	#07
		sta	temp2
		lda	_cmode
		eor	#02				;Do it display all 0's
		jsr	dis01				;Display
		lda	#-$3F
		ldx	#-$0A
		jsr	vgvtr5			;Position for next line
		lda	#07
		sta	temp2
		jsr	getswitch			;Get switches
		jsr	dis01				
		jsr	getswitch
		and	#$40
		ifne
			asl	temp9+1			;Pressed long enough
			ifcs
?st410			lda	nxtsnd
				ifeq
					lda	#$0D				;should be a #0C if statst included
				else
					clc
					adc	#01
					cmp	#$48
					ifeq
						lda	#00
						sta	temp9+3
					endif
				endif
				sta	nxtsnd
				lda	#00
				sta	temp9+1			;Don't step right away again
			endif
		else
			inc	temp9+3		;Hold down counter
			lda	temp9+3		
			and	#$1F
			beq	?st410
			lda	#$20
			sta	temp9+1		;Debounce timer
		endif
		jsr	getswitch
		ifmi
			asl	temp9+2
			ifcs
				lda	nxtsnd
				cmp	#$0D			;Clear high scores??
				bne	?st422
				;Stats clear only needs coin switch too
				jsr	getc			;Get coin switch
				;A and X = switch, sigh set by a TXA
				bmi 	?st425		;Skip it for now
				lda	#$12
				jsr	dosnd2		;Make a noise
				jsr	dodelay		;Wait a bit
?st422			lda	nxtsnd
				jsr	dosnd2		;Start this one
			endif
		else
			lda	#$20
			sta	temp9+2		;Reset press counter
?st425	endif
		;Now display information
		jsr	inilit			;Keep getting options
		ldy	demo				;Which mode?
		ldx	diftbl,Y
		jsr	mesg
		ldx	#mac10			;Guess adapt on
		lda	addap				
		ifeq
			ldx	#mac11
		endif
		jsr	mesg
		ldx	#mac12			;Guess sounds in attract
		lda	sndatt
		ifeq
			ldx	#mac13
		endif
		jsr	mesg
		lda	_cmode
		and	#03
		tay	
		ldx	cmodem,Y			;Message from Mainline
		jsr	mesg				;Display cost
		lda	#$80
		sta	_tcmflg			;Xfer for possible display
		lda	#00
		sta	_crdt
		jsr	doptn				;Display bonus and lives
		jsr	vgcntr
		lda	#-8
		ldx	#-$28
		jsr	vgvtr5			;No place for LETA
		jsr	getleta			;Display direct
		sta	temp2
		lda	#temp2			;So Display can use it
		ldy	#01
		jsr	digits			;Display this
		jsr	vgcntr			;Display song number
		lda	nxtsnd
		ifne
			sec	
			sbc	#$0B				;Don't display 01-0B
		endif
		pha	
		cmp	#07				;Sound starts
		ifcs					;A Sound??
			lda	#06				;Use this message
		endif
		tay	
		ldx	st_tmesg,Y
		jsr	mesg				;Display message
		pla					;See if a sound
		beq   ?st440			;0 Displays
		cmp	#01				;EEROM stuff??
		beq	?st420			;Skip number
		cmp	#02
		beq	?st420			;Same!
		cmp	#05
		beq	?st420			;Skip for sound off too!
		ifcc					;Has to be left and right
			ldy	#$38				;Guess right
			cmp	#03				;Is it 3?
			ifeq
				ldy	#$2C				;It's left
			endif
			lda	vgmsga,Y
			ldx	vgmsga+1,Y
			jsr	vgadd2			;Add the letter
		else
?st440		sta	temp2
			lda	#temp2
			ldy	#01
			jmp	digits			;Display this song
		endif
?st420	rts
	
gamnws	.byte $1E,$34,$32,$38
st_tmesg	.byte mactb,mact8,mact7,mact9,mact9,macta,mactb
diftbl	.byte mactd,macte,mactf,mactc

;**********************************************
	.sbttl "BIPS Check"
;**********************************************
stest5	lda	#$A4				;A few red ones
		ldx	#spot7
		jsr	vgadd2			;Set color red
		lda	#00
		ldx	#$73				;This scale
		jsr	vgadd2
		jsr	vgcntr
		laljsr(frbox)			
		lxhjsr(frbox)			
		jsr	vgadd2			;Box with center and position
		lda	#05
		sta	temp3
		begin
			laljsr(frbx2)
			lxhjsr(frbx2)			;Box without center and scale
			jsr	vgadd2
			dec	temp3
		miend
		lda	#$A2
		ldx	#spot7
		jsr	vgadd2			;Change color on last one
		laljsr(frbx2)
		lxhjsr(frbx2)
		jmp	vgadd2			;Add last box
		
;************************************************
	.sbttl "Report Problems and Display Switches"
;************************************************
stest6	lda	#$F7
		ldx	#spot7
		jsr	vgadd2
		jsr	vgcntr			;This to change later (no RAM if possible)
		lda	#-$40
		ldx	#-$20
		jsr	vgvtr5
		lda	#01				;Scale 1
		jsr	vgsca1			;Big Numbers
		jsr	optn2				;Output option switches
		lda	#02
		jsr	vgscal			;Return to normal scale
		ldx	#$46
		stx	temp2+1			;Starting Y value checksums
		ldx	#10d
		begin
			lda	pnttbl,X
			ifne
				stx	temp2				;Save checksum #
				jsr	vgcntr			;Center beam
				ldx	temp2+1
				txa	
				sec	
				sbc	#08				;32d below current line
				sta	temp2+1
				lda	#-10d
				jsr	vgvtr5			;Position beam
				lda	temp2
				jsr	vghex				;ROM #
				lda	#24d/4
				ldx	#00
				jsr	vgvtr5
				lda	temp2
				clc	
				adc	#pnttbl
				ldy	#01
				jsr	digits			;Display two digits
				ldx	temp2
			endif
			dex
		miend
		jsr	vgcntr
		lda	#-10d
		ldx	#$50
		jsr	vgvtr5			;Position for error list
		ldx	#04
		stx	temp2
		begin
			ldx	temp2
			ldy	#00
			lda	erplc,X			;Any bad news?
			ifne
				ldy	badnews,X
			endif
			lda	vgmsga,Y
			ldx	vgmsga+1,Y			;Get letter
			jsr	vgadd2
			dec	temp2
		miend
		jsr	vgcntr			;ROM switch test
		lda	pnttbl+1
		ora	pnttbl+2
		ora	pnttbl+3
		ora	pnttbl+4			;Any page errors on vectors?
		ifeq					;If yes, skip this
			lda	#-$18
			ldx	#-$48
			jsr	vgvtr5			;Position
			lda	#00
			ldx	#$72
			jsr	vgadd2
			ldx	#03
			stx	temp3
			begin					;Output 3 numbers
				ldy	temp3
				lda	#$F7
				ldx	pgsel,Y			;Get stat for each page
				jsr	vgadd2			;Add stat
				laljsr($6000)
				lxhjsr($6000)			;JSRL to 6000 in this page
				jsr	vgadd2
				laljsr(char_space)
				lxhjsr(char_space)		;Put a space
				jsr	vgadd2
				dec	temp3				;Done all 4?
			miend
		endif
		rts
			
pgsel		.byte $60,$61,$62,$63		;Stats for page select

;***********************************************
	.sbttl "Pattern Test and Alphabet (Tell a Friend)"
;***********************************************
stest7	laljsr(crosshatch)
		lxhjsr(crosshatch)
		jmp	vgadd2

;***********************************************
	.sbttl "Hysterisis"
;***********************************************
stest8	laljsr(hystr)
		lxhjsr(hystr)
		jsr	vgadd2
		jsr	vgcntr
		lda	frame+1
		sta	temp3				;Hold here to use
		lda	frame
		rol	A
		rol	temp3				;Use D7,D1,D0
		lda	temp3
		and	#07				;Binary part
		ora	#$70				;Make look like a stat
		cmp	#$71				
		ifcc					;Skip if too big
			rts
		endif
		tax	
		lda	frame
		and	#$7F
		jsr	vgadd2			;Write stat instruction
		laljsr(frbox)
		lxhjsr(frbox)
		jmp	vgadd2
		
;***********************************************
	.sbttl "Color Bars Test 1 (Old Test)"
;***********************************************
stest9	lda	#02				;Set scale 2
		jsr	vgsca1			;Set scale
		ldy	#06
		sty	temp3
		begin
			jsr	vgcntr			;Center
			ldy	temp3
			lda	posix,Y			;Position this group
			ldx	posiy,Y
			jsr	vgvtr5
			lda	temp3
			eor	#$FF
			and	#07
			ldx	#spot7
			jsr	vgadd2
			lda	temp3				;White group?
			ifeq
				laljsr(clpt2)
				lxhjsr(clpt2)			;Special White group
			else
				laljsr(clpat)
				lxhjsr(clpat)
			endif
			jsr	vgadd2
			dec	temp3
		miend
		laljsr(cl73)				;Last on is white
		lxhjsr(cl73)
		jmp	vgadd2

;**************************************************
	.sbttl "Color and Stat Check"
;**************************************************		
stesta	jsr	vgcntr			;Center
		lda	#02				;Set scale to 2
		jsr	vgsca1			;Set scale
		bit	temp2				;Already written pattern to buffer?
		ifpl
			jsr	initcol			;Init color RAM
			lda	vglist
			pha	
			lda	vglist+1
			pha					;Save current pattern
			lda	#(vecram+$400)&$ff	
			sta	vglist			;Use upper half of RAM
			lda	#((vecram+$400)&$ff00)/$100
			sta	vglist+1			;Set up buffer pointer
			ldy	#16d-1			;Number of patterns
			sty	temp3
			lda	#$82
			ldx	#$20				;Move to left to draw a pattern
			jsr	vgvtr5
			begin
				lda	#$F0				;One line of each intensity
				sta	temp3+1			;Save intensity
				sta	watchdog
				begin					;Draw a group
					lda	temp3+1			;Intensity of this line
					ora	temp3				;Add in color of this line
					ldx	#$60				;Stat instruction
					jsr	vgadd2
					ldy	#$20
					lda	#$0C
					ldx	#00
					jsr	vgvtr				;Small line
					lda	#-$0c
					ldx	#-4				
					jsr	vgvtr5			;Position for next line
					lda	temp3+1
					sec	
					sbc	#$10				;Next Intensity
					sta	temp3+1
				ccend					;Done with that group
				lda	#$11				;Move over for next group
				ldx	#$40				;Move up to beginning
				jsr	vgvtr5
				dec	temp3				;Next group??
			miend
			jsr	vgrtsl			;Add an RTSL to buffer
			pla	
			sta	vglist+1			;Restore old vglist
			pla	
			sta	vglist
			lda	#$80
			sta	temp2				;Set flag
		else
			laljsr(vecram+$400)		;Add JSRL to list
			lxhjsr(vecram+$400)
			jmp	vgadd2
		endif
		rts	
		
;*******************************
;* X Position for bars
posix		.byte $DE,$9D,$1F,$9D,$DE,$1F,$DE

;*******************************
;* Y Position
posiy 	.byte $F4,$D8,$D8,$10,$D8,$10,$10

badnews	.byte $16,$18,$1A,$1C,$1E

;******************************************
	.sbttl "Checker Board"
;******************************************
stst10	jsr	vgcntr
		lda	#02
		jsr	vgsca1
		ldx	#09				;Nine bars horizontal
		stx	temp2
		jsr	vgcntr
		begin
			ldy	temp2
			lda	#$80
			ldx	hlpos,Y
			jsr	vgvtr5			;Postion for this line
			laljsr(hline)
			lxhjsr(hline)			;Draw line and center
			jsr	vgadd2
			dec	temp2
		miend
		ldx	#11d				;Number of Bars
		stx	temp2
		begin
			ldy	temp2
			lda	vlpos,Y
			ldx	#$6C
			jsr	vgvtr5
			laljsr(vline)
			lxhjsr(vline)
			jsr	vgadd2
			dec	temp2
		miend
		lda	gamerr			;Gamma Running?
		ifpl					;Nope, use coin switch
			jsr	getc				;Returns A=X and sign set of A
			ifmi
				asl	tempa				;Debounce color switch
				ifcs
					inc	temp8				;Next color
				endif
			else
				lda	#$20				;Reset not pushed
				sta	tempa
			endif
		else					;Gamma OK, use gamma
			jsr	getleta			;Get LETA value
			lsr	A
			lsr	A
			lsr	A
			lsr	A				;Use top 4 bits
			sta	temp8				;Sav for color
		endif
		rts	
		
getc		lda	#$0C				;Deselect player 1 for coins
		sta	inverts
		lda	_coina
		ldx	#plrslb+$0C
		stx	inverts			;Set for coin switch
		tax					;Test sign of A
		rts	
		
hlpos		.byte $A1,$B6,$CB,$E0,$F6,$0A,$20,$35,$4A,$5F

vlpos		.byte $94,$A8,$BB,$CF,$E3,$F6,$0A,$1D,$31,$45,$58,$6C

;********************************************
	.sbttl "Display Option Switches"
;********************************************
optn2		lda	#07				;16 Switches
		sta	temp2
		lda	#$0C				;Drop player select for these switches
		sta	inverts
		lda	ststsw
		jsr	dis01
		lda	#-$40
		ldx	#$F8
		jsr	vgvtr5			;Position for next line
		ldx	#07
		stx	temp2
		sei					;No interrupts while reading switch
		lda	#plrslb+$0C			;Reselect self test stuff
		sta	inverts
		lda	ststsw			;Remaining switches
		cli	
		jmp	dis01
		
dis01		begin
			pha	
			and	#01
			jsr	vghex				;Display 0 or 1
			pla	
			ror	A
			dec	temp2
		miend
		rts
		
;*****************************************************
	.sbttl "Self Test Main Line"
;*****************************************************	
diagm		begin
			lda	frame
			and	#$10
			lsr	A
			lsr	A
			lsr	A
			lsr	A
			ora	#$0C				;Keep processor on
			sta	out1s				;For interrupt stuff
			ldx	#40d
			begin
				begin
					lda	ststsw
					lsr	A
					lsr	A
				csend
				begin
					lda	ststsw
					lsr	A
					lsr	A
				ccend
				dex
			miend
			inc	frame
			ifeq
				inc	frame+1			;2 Bytes for bin scale test
			endif
			begin
				lsr	halt				;Stopped?
			csend
vgld			sei					;Hold interrupts until done with write
			lda	#(vecram&$ff)		;Reload VG
			sta	vglist
			lda	#(vecram&$ff00)/$100	
			sta	vglist+1
			lda	diagsw
			and	#diagbit			;Switch pushed?
			ifne
				asl	temp9
				ifcs					;Pressed long enough?
					lda	#00
					sta	temp2				;Buffer flag clear for some routines
					sta	nxtsnd			;Start again at 0
					inc	testnm
					inc	testnm			;Next test
					lda	#00
					ldx	#06
					begin
						dex
						dex
					miend
				endif
			else
				lda	#$20
				sta	temp9				;Not pressed, restart timer
			endif
			jsr	vgcntr
			lda	#00
			sta	xcomp
			sta	xcomp+1			;Setup for window
			sta	xcomp+2
			sta	vgbrit
			ldx	#$71
			jsr	vgadd2			;Set scale
			lda	#01
			sta	xcomp+3			;VCTR 100 up
			jsr	vgvtr2
			lda	#$79
			tax					;Set window (x=79)
			jsr	vgadd2
			lda	#$F4
			ldx	#spot7
			jsr	vgadd2
			jsr	vgcntr
			laljsr(frbox)
			lxhjsr(frbox)
			jsr	vgadd2			;Add red box
			jsr	vgcntr
			lda	testnm
			cmp	#sftjse-sftjsr-2		;Last test
			ifeq
				lda	temp8
				and	#07
				ifeq
					lda	#01
				endif
				ora	#$C0				;Intensity
			else
				lda	#$F7				;Put up white box
			endif
			ldx	#spot7
			jsr	vgadd2
			laljsr(frcfl)
			lxhjsr(frcfl)  
			jsr	vgadd2			;Do box
			jsr	dostate			;Do this routine
			jsr	vgcntr
			jsr	vghalt			;Center and halt
			sta	vggo				;Start display
			sta	watchdog
			lda	#plrslb+$0C			;Player select bit (keep gamma running)
			sta	plysel			;Select player 1
			lda	ststsw			;Still Self test??
			cli
			and	#ststbit
		neend
wdreset	bne	wdreset			;Watch dog reset for exit


;***********************************************
	.sbttl "Test Cases"
;***********************************************
sftjsr	.word	stest4-1		;Gamma Test Stuff
		.word stest6-1		;Display Switches and Report problems
		.word stest8-1		;Hysterisis
		.word stest5-1		;BIPS check
		.word stest7-1		;Cross hatch and Alpha
		.word stest9-1		;Color Bars
		.word stesta-1		;Intensity check
		.word stst10-1		;Hatch
		
sftjse
dostate	ldx	testnm
		cpx	#sftjse-sftjsr		;Non valid state?
		ifcs
			ldx	#00
			stx	testnm			;Start Over!
		endif
		lda	sftjsr+1,X
		pha	
		lda	sftjsr,X
		pha	
		rts				;Jump to case

;***********************************************
	.sbttl "RAM Test Routines"
;***********************************************
;* Assumes Page 0 has been tested and is good! *

vram		=	$0900		;Use 900 page, we know it is good
soft		=	1		;Will specify start and stop addresses
tstart	=	$9F0
tend		=	$9F2
ramwid	=	8		;RAM's are 8 bits wide
speed		=	1

;**********************************************************
;* Entry: (A)  = MSB of starting address                  *
;*                                                        *
;* Exit:  (CC) = Carry set if error occured               *
;*        (A)  = Difference between expected and recieved *
;**********************************************************

tstram	sta	tstart		;Save MSB of address
		clc	
		adc	#$0F			;Add for end of 4K x 8
tst2k		sta	tend

;**********************************************************
	.sbttl "Generic RAM Test Code"
;**********************************************************

ramtst	ldx	#00			;Enter here to test RAM
nxtpat	ldy	#vtend-voltbl
?grt10	lda	voltbl,Y		;Move volitile code into RAM
		sta	vram,Y
		dey	
		bpl	?grt10
		lda	tstart
		sta	vadh1
		sta	vadh2
		sta	vadh3
		lda	tend
		sta	vadh4			;Set up 'soft' starting address
		txa	
		bne	?grt20
		stx	vpat			;No comparison on first pass
		ldx	#patend-pats+1
		bne	?grt30		;Always
?grt20	dex	
		txa				;Return with zero if done	
		beq	endtst
?grt30	lda	pats-1,X
		ldy	pats,X
		jmp	vram
		
;************************************************************
	.sbttl "Crashable Code"
;************************************************************

voltbl	sta	$100

vadl1		= 	*-voltbl-2+vram
vadh1		=	*-voltbl-1+vram

		cpy	$101
		
vadl2		=	*-voltbl-2+vram
vadh2		=	*-voltbl-1+vram

		bne	?cc50
	
vpat		=	*-voltbl-1+vram			;Set to zero for the inital pass

		cmp	$100
		
vadl3		= 	*-voltbl-2+vram
vadh3		=	*-voltbl-1+vram

		bne	?cc50				;RAM Failure
		inc	vadl1
		bne	?cc10
		inc	vadh1
?cc10		inc	vadl2
		bne	?cc30
		inc	vadh2
		sta	watchdog
		ldy	tend
		cpy	vadh2
		bcs	?cc20
		sta	$01FF				;Set last byte for next pass
		
vadh4		=	*-voltbl-1+vram

		jmp	nxtpat
?cc20		ldy	pats,X
?cc30		inc	vadl3
		bne	?cc40
		inc	vadh3
?cc40		jmp	vram
?cc50		sec	
		jmp	ramerr

vtend
pats		.byte $00,$FE,$01,$FD,$02,$FB,$04,$F7,$08
		.byte $EF,$10,$DF,$20,$BF,$40,$7F,$80
		
patend	.byte $ff				;Start with ff and end with RAM set to 00

endtst	clc

ramerr	tsx	
		inx
		ifne				;Can't use rts if page 1xx as we jumped here
			rts
		endif
		; We are doing 1 page, skip rts and jump back to rest of test
		jmp	zpgtst

cksum6	.byte $F7
		
;*************************************************
	.title "Morse Code"
	.sbttl "Copyright 1983 Atari"
;*************************************************
		.byte $02,$bb,$5a,$30,$05,$ee,$0d,$a8


;******************************************
;* Don't forget to do the entry points!!! *
;******************************************	
	.org $fffa
	
		.word pwron
		.word pwron
		.word irq

	.end

;****************************************
;* Global Exports
;****************************************
; Temps
.export temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,tempa
.export perm1,perm2,perm3,perm4,perm5

.export updflg,frame,wrhflg,pl1last,pl2last,digits,hscore,xcomp

.export mesg,updwho,flsflg,updint,rgdr,wrpdl,initl,jblast,jbstat

; Vector Routines
.export vgcntr,vgadd2,vgcntr,vgvtr5,vgadd,vgvtr2,vgbrit,vgvtr,vgscal,vgjmpl
.export vgjsrl,vgvctr,vgreset,vgrtsl,vgsca1,vgstat

.export vecram
.export player,score,dostop,dodelay,dosound,cabsel,out1s,bonusa,shipst
.export init3,webscur,webssta,websnum,webss2
.export map0,map1,map2,map3,map4,map5,map6
.export game1,game2,atime1,atime2,warps,extlie,dblneg,watchdog
.export spdt,prepare,dist,noneleft,statyl,statyh,linmult,decimal,incdif

.export hxtend,hytend,crsbuf,mapbuf,retbuf,timbuf,scobuf,scobuf2,scrbuf,trnbuf
.export qrtlog,fullog,fullog2,sin,cos,tstat,scbdis,sobjst,trinds,tcount,piccur,mzgame
.export linscal,lauen,lincur,plysel,nextmz,manstat,mancol2,nxtdly,olmznm,target
.export tactde,nxtptr,colcnt,bronce,statst,tact,rearview
.export websxl,websxh,websyl,websyh
.export difcty,webmult,sndcue,spcspd,websper,websseg
.export scalef,onscreen,hitpts,hitwebs,vglist,gridx,gridy
.export shotxl,shotxh,shotyl,shotyh,shtspd,shotcur,shotst
.export shipxl,shipxh,blowship,multiply
.export getrand,cxflip,posvc2,nmsshots,bpont2,linen
.export objst,zreactor,retime,tspark,nodraw,getptr
.export mazexl,mazexh,mazeyl,mazeyh,neg,xoffset,ytop,vunits,ymot,xmot,mazpt
.export holmz,openflg,maznum,gamest,mzgrnd,mazer,dif4mz,unitp,hunits

; Maze Objects
.export nmfire,zfire,nmrob,zrobot,nmlock,nmkeys,zlock,zkeys,nmobj
.export nmlsht,zlsht,nmcann,zcann,nmshot,zshot,nmtite,ztite,nmtran,ztran
.export nmstuf,zstuf,nmdisc,zdisc,nmligh,zligh,nmfrfl,zfrfl,nmtrpp,ztrpp
.export nmonew,zonew,nmarow,zarow,nmsparks
.export onewst,epodfr,epodgr,ardir,cannfr,cannss,canngr,canndf,cannin,cannp1,cannp2

.export mestim,jmptim,fldcnt,jumprv,tranhi,cktran,ntrans,ttran
.export naccx,naccy,maccx,maccy,daccx,daccy,raccy,docase,topcse,stasav
.export mzgms,initshp,score2,wrplvl,scrflg,brick,stbflg,statxl,statyl
.export reacst,maxdif,objxl,objxh,objyl,objyh,objfrm

; Colors
.export shtcol,fircol,headcol,mancol,mazcol,ltcol,rtcol,robcol,stcolr,initcol
.export green,black

.export lsbsy,lsbsx,zindex,stloc
.export oldxl,velxh,velyh,robdir,robvel,limbo,reintn,rodstat,xflip
.export nxtexp,sparkle,sparkb,blank,msgnop,inblow,sparkangle
.export cannon,sldnfr,nxtdisc,accbuf,thisarw,zspecial
.export obssxl,obssxh,obssyl,obssyh,ztop,ground,rampg
.export obsst,shipdis2,ctran,xtran,ytran,colram

.export litra2,litra3,foreign,strtst,strtyl,strtyh,strtln,slives,_cmode

;****************************************
;* Message Exports
.export mplayr,mgtsc,menin,mact4,mact4a,mgetout,mhis

;****************************************
;* Color Indexes
.export colblack,colblue,colgreen,colcyan,colpurple,colyellow,colwhite,colwhiter,colpink,colorange,colredr,colred,colcyanr,colbluer,colgreenr
