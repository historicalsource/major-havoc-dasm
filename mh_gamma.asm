;***************************************************************
;* Major Havoc MainLine Program (Gamma Processor)              *
;***************************************************************
	.title "Gamma Processor"               
;*************************************************************** 

.locallabelchar "?"
.include "logic.ah"
.include "vector.ah"

;***********************************************************
	.sbttl "Declaration of RPM Variables"
;***********************************************************
___tlk	= 	0	;SPEECH
___imm	=	1	;IMMEDIATE Mode Functions
___nmi	=	1	;NMI port control
___exm	=	1	;support EXCEPTION mode		
			
			
;* Device Declarations - This allows the macro STSND to start
;*                       logical named channels more easily.
numpokey	=	4

pokeyc0	=	0	;Channel Offsets
pokeyc1	=	4
pokeyc2	=	8
pokeyc3	=	12

;* 4 Channels per Pokey
numchannel	=	4*numpokey

;* Declare all tables large enough. The macros will fill in
;* the entries....
;*
;* ***** Table Limits *****
;* 
;* Byte Tables: these are addressed using Y as an index, so 
;*              they are limited to 256d entries.

numcom	=	$c0		;Total Number of legal commands
numsnd	=	$60		;MAX number of sound groups
numtun	=	$80		;MAX number of tunes to group
numimm	=	$10		;MAX number of immediates
numtlk	=	$20		;MAX number of speech samples

;************* Some quick hacking tables *******************
maxtunecount	= $7A
maxnumdat1		= $68
maxnumdat2		= $68

;* Word Tables: these are limited to 128d entries for the 
;*              same reason.

numexc	=	15		;MAX exceptions
numfcns	=	8		;Number of legal user functions

;************************************************************
;* This is the end of the User Configuration Section        *
;************************************************************

;***********************************************************
;*   Variables and Hardware Allocation                     *
;***********************************************************

savecnt	=	3		;Number of High Scores Saved
numhs		=	savecnt*4
numit		=	savecnt*3

;Export Above Variables
.export numhs,numit


g_tout	=	$02		;Signals that Gamma has timed out
g_done	=	$03		;Signals that Gamma is done sending stuff	

qsize		= 	$80		;Max size of the roundabout command queue
tibufsize	=	$08		;Max size of of the speech command queue

counter1	= 	1		;Byte Location of Coin Counter 1
counter2	=	2		;Byte Location of Coin Counter 2

; EEROM Flags
eeras		=	$80
eewrit	=	$40
eeread	=	$20

;Input Bit Definitions
alphaxmtd	=	$01		;Alpha Processor Transmitted
alpharcvd	=	$02		;Alpha Processor Ready to Recieve
tirdyflg	=	$04		;TI Speech Chip Ready

	.sbttl "Hardware Equates"

pokey1	= 	$2000
pokey2	=	$2008
pokey3	=	$2010
pokey4	=	$2018
random	=	$002a
audctl	=	$0020
potgo		=	pokey1+$23
allpot	=	pokey1+$20
portst	=	$2800
input		= 	$2800
tirdy		=	$2800
indata	=	$3000
leta		=	$3800
intack	=	$4000
counter	=	$4800
outdata	=	$5000
tiwson	=	$5800
tiws	=	$5801
eerom		=	$6000

;*****************************************************************
;* RAM Allocation                                                *
;*****************************************************************
.include "g_ram.ah"

;**********************************
;* TWGamma MainLine               *
;**********************************
	.title "TWGamma"
	.sbttl "MainLine"
	.org $8000
 
pwron	sei	
		cld	
		ldx	#$FF			;Set Stack Pointer 
		txs	
		jsr	r_sysst		;Test the Gamma system
cindy	pha				;Save the result.
		ldx	#-1
		stx	datnum		;Set NMI to command mode
		inx	
		stx	framecnt
		stx	irqcnt
		stx	r_sysi
		stx	gw_i			;Reset the Alpha Write Queues
		stx	gw_ia
		stx	gr_i			;Reset the Alpha Read Queues
		stx	gr_ia
		lda	indata		;Read a garbage byte from Alpha just in case it is waiting
		pla				;Get Self Test result from above
		sta	outdata		;Send it to the Alpha (Should be -1 if all okay)
c_reset	jsr	isnd			;Initialize all RAM etc.
		cli	
		
;************ Begin Main Process Loop ***********************

mainloop	lda	#alpharcvd
		bit	portst		;Alpha Ready?
		ifne				;yes
			ldy	gw_i
			cpy	gw_ia			;Check for anything in queue
			beq	?ml10			;If not, then skip writing
			lda	gw_queue,Y		;Get the next command
			sta	outdata		;Write it!
			iny	
			cpy	#qsize			;Max 80 commands
			ifcs
				ldy	#00			;Loop the queue
			endif
			sty	gw_i			;Store it
		endif
?ml10		ldx	gr_i
		cpx	gr_ia			;Check the read queue
		ifne				;Something came in!
			inx	
			cpx	#qsize
			ifcs
				ldx	#00			;Reset the queue
			endif				
			stx	gr_i			;Save new queue location
			ldy	gr_queue,X
			jsr	scomm			;Go to it!
		endif
		jsr	framecntl		;Update the Coin Counters	
		jmp	mainloop		;Loop it!
		
;************************************************************
; Include RPM                                               *
;************************************************************
rpm_base = *
#include "rpm.asm"


;******************************************************
	.sbttl "Build Sound Command Tables"
;******************************************************
;* The RPM macros etc will automatically build all    *
;* the necessary tables for each command.             *
;******************************************************
; Define the various tunetables

	STSND(g_snd_fak,repl,7,-1,0,s_mzassem2)	;Unknown Sound
	EXCEPT(g_geth,gets)
	EXCEPT(g_geti,getin)
	EXCEPT(g_sendh,sendhs)
	EXCEPT(g_sendi,sendinit)
	EXCEPT(g_gsta,getsta)
	EXCEPT(g_ssta,sendst)
	EXCEPT(g_ctrl,rolgig)
	EXCEPT(g_rand,pokran)
	EXCEPT(g_swtc,switch)
	EXCEPT(g_opt0,option0)
	EXCEPT(g_opt1,option1)
	EXCEPT(g_clrh,newscr)
	EXCEPT(g_clrs,newst)
	EXCEPT(g_cn1,coin1)
	EXCEPT(g_cn2,coin2)
	KILALL(snd_stop)
	
;* Sound Effects
;*
;* A) Game Start
	STSND(snd_a1,repl,8,-1,08,s_coin)		;Coin
	STSND(snd_a2b,repl,8,-1,08,s_launch)	;Launch
	 CONT(             8,-1,15,s_launch2)
	STSND(snd_a2b2,repl,8,-1,08,s_passby)	;Passby
	 CONT(              8,-1,09,s_passby2)
	 
;* B) Tactical Scanner
	STSND(snd_b1a,repl,8,-1,11,s_galert)	;General Alert
;	STSND(snd_b1c,repl,8,-1,11,s_alert1)	;Alert1
;	STSND(snd_b1d,repl,8,-1,11,s_alert2)	;Alert2(reactor hum)
	STSND(snd_b1e,repl,8,-1,11,s_redalert)	;Red Alert
     	STSND(snd_b2a,repl,8,-1,08,s_blaunch)	;Ball Launch
     	STSND(snd_b2b,repl,8,-1,08,s_bbrick)	;Ball Hits Brick
     	STSND(snd_b2c,repl,8,-1,08,s_bpaddle)	;Ball Hits Paddle
     	STSND(snd_b2d,repl,8,-1,08,s_bmissp)	;Ball Misses Paddle
     	STSND(snd_b3a,repl,8,-1,09,s_digit)	;Digit Entered
     	STSND(snd_b3b,repl,8,-1,08,s_ccorrect)	;Combination Correct
     	
;* C) Shared Sounds for All Space and Maze Waves
	STSND(snd_c1,repl,8,-1,15,s_exp)		;Player Ship Explosion
	 CONT(            8,-1,00,s_exp2)	
	 CONT(            8,-1,01,s_exp3)
	 CONT(            8,-1,14,s_exp3)
	STSND(snd_c2,repl,8,-1,02,s_pfire)		;Player Ship Firing
	STSND(snd_c3,repl,8,-1,03,s_shotstat)	;Shot Hits Station
	STSND(snd_c4,repl,8,-1,04,s_xlife)		;Bonus Life
	 CONT(            8,-1,03,s_xlife)
	STSND(snd_c5,repl,8,-1,03,s_bonus)		;Bonus Tick
	STSND(snd_c7,repl,8,-1,08,s_beeps)		;75 Bonus Ticks
	 KILID(snd_c6,snd_c7)
;	STSND(snd_c6,repl,8,-1,12,s_bassboom)	;Bassy Boom
;	 CONT(            8,-1,13,s_bassboom2)	
;	STSND(snd_c7,repl,8,-1,04,s_ataritune)	;Atari Tune
;	 CONT(            8,-1,05,s_ataritune2)
;	
;* D) Robot Fish Space Wave
	STSND(snd_d1,repl,8,-1,08,s_fishhatch)	;Fish Hatch
;	 CONT(            8,-1,09,s_fishhatch2)
;	 CONT(            8,-1,10,s_fishhatch3)
;	STSND(snd_d2,repl,8,-1,06,s_reactor)	;Background(Reactor Supercritical)
;	 CONT(            8,-1,07,s_reactor2)
	STSND(snd_d3,repl,8,-1,08,s_goosefish)	;Goose Fish
	STSND(snd_d4,repl,8,-1,09,s_blowfish)	;Blow Up Fish
	 CONT(            8,-1,10,s_blowfish2)
	STSND(snd_d5,repl,8,-1,11,s_circfish)	;Circling Fish
	 CONT(            8,-1,05,s_circfish2)
	 
;* E) Galaxians Space Wave
	STSND(snd_e1,repl,8,-1,08,s_feject)	;Fighters Leave Station
	STSND(snd_e2,repl,8,-1,09,s_fshot)		;Fighter Shoots
	STSND(snd_e3,repl,8,-1,10,s_exp3)		;Blow Up Fighter
	 CONT(            8,-1,14,s_exp5)		
	STSND(snd_e4,repl,8,-1,10,s_exp3)		;Blow Up Fighter 2
	 CONT(            8,-1,09,s_exp4)
	 CONT(            8,-1,14,s_exp6)
	 
;* F) Web Spinners Space Wave
;	STSND(snd_f1,repl,8,-1,06,s_mzassem)	;Maze Assembly
;	 CONT(            8,-1,07,s_mzassem2)
;	 CONT(            8,-1,11,s_mzassem3)
	STSND(snd_f2,repl,8,-1,08,s_blowspin)	;Blow Up Spinner
	STSND(snd_f3,repl,8,-1,09,s_mazehit)	;Hit Maze Segment
	STSND(snd_f4,repl,8,-1,09,s_mazekill)	;Blow Up Maze Segment
	 CONT(            8,-1,10,s_mazekill2)
	 
;* G) Red Line Space Wave
;	STSND(snd_g1,repl,8,-1,06,s_lineback)	;Background
;	STSND(snd_g2,repl,8,-1,07,s_lineback)	;Background
	 
;* H)	Landing on the Space Station
	STSND(snd_h1,repl,8,-1,15,s_exp5)		;Successful Landing
	STSND(snd_h2,repl,8,-1,15,s_exp6)		;Crash Ship on Station
	 CONT(	       8,-1,08,s_exp3)
	 CONT(            8,-1,09,s_exp7)
	STSND(snd_h3,repl,8,-1,09,s_lshot)		;Laser Shot
	
;* I) Maze Wave
	STSND(snd_i1a,repl,8,-1,04,s_shield)	;Shields in Use
	STSND(snd_i1b,repl,8,-1,04,s_hitshield)	;Item Hits Shield
	STSND(snd_i1c,repl,8,-1,04,s_noshield)	;Shields Used Up
	STSND(snd_i2a,repl,8,-1,09,s_manhit)	;Man Hits Object
	 CONT(	        8,-1,10,s_shotstat)
;	STSND(snd_i2b,repl,8,-1,09,s_manhit)	;Man Hits Electric Fence
;	 CONT(             8,-1,10,s_shotstat)		
	STSND(snd_i2c,repl,8,-1,11,s_manwall)	;Man Hits Wall/Ceiling
	STSND(snd_i2d,repl,8,-1,11,s_mantrip)	;Man Hits Trip Pad
	 CONT(	        8,-1,04,s_mantrip)
;	STSND(snd_i2e,repl,8,-1,08,s_manswon)	;Man Uses Switch - On
;	STSND(snd_i2e2,repl,8,-1,08,s_manswoff)	;Man Uses Switch - Off
	STSND(snd_i2f,repl,8,-1,00,s_spikes)	;Man Hits Stalactite
	 CONT(             8,-1,01,s_spikes2)
	 CONT(             8,-1,02,s_spikes3)      
	STSND(snd_i2g,repl,8,-1,08,s_oxygen)	;Man Picks Up Oxygen
	STSND(snd_i2h,repl,8,-1,00,s_key)		;Man Picks Up Key
	STSND(snd_i2i,repl,8,-1,00,s_door)		;Man Opens Door
;	STSND(snd_i3a,repl,8,-1,07,s_rhum)		;Reactor Hum
	STSND(snd_b1d,repl,8,-1,11,s_alert2) 	;Reactor Hum
;	STSND(snd_i3b,repl,8,-1,07,s_reactor3)	;Reactor Supercritical
	STSND(snd_d2,repl,8,-1,06,s_reactor)	;Reactor Supercritical
	 CONT(             8,-1,07,s_reactor2)
;	STSND(snd_i4a,repl,8,-1,00,s_handact)	;Hand in Action
;	 CONT(             8,-1,01,s_handact2)
	STSND(snd_i4b,repl,8,-1,02,s_handoff)	;Hand Turned Off
	STSND(snd_i4c,repl,8,-1,02,s_handon)	;Hand Turned On
;	STSND(snd_i5,repl, 8,-1,04,s_tickfast)	;Clock Ticking - Fast
;	STSND(snd_i52,repl,8,-1,04,s_tickslow)	;Clock Ticking - Slow
	STSND(snd_i6,repl, 8,-1,00,s_trans)	;Transporter Booth
	 CONT(             8,-1,02,s_trans2)
;	STSND(snd_i6b,repl,8,-1,00,s_trans)	;Transporter Booth(Louder)
;	 CONT(             8,-1,02,s_trans2)
;	 CONT(             8,-1,04,s_trans3)
;	 CONT(             8,-1,06,s_trans4)
	STSND(snd_i7a,repl,8,-1,01,s_robshot)	;Robot Fires a Shot
	STSND(snd_i7b,repl,8,-1,08,s_cann)		;Laser Cannon Fires
	 CONT(             8,-1,09,s_cann2)
	 CONT(             8,-1,10,s_cann3)
	 CONT(             8,-1,11,s_cann4)
	STSND(snd_i7c,repl,8,-1,01,s_ssplash)	;Shot Splashes On Wall
	STSND(snd_i8,repl,8,-1,00,s_footstep)	;Footsteps
	STSND(snd_i9,repl,8,-1,07,s_nooxy)		;Oxygen Out
	
;* J) Leaving the Maze
;	STSND(snd_j2,repl,8,-1,08,s_launch)	;Ship Blasts Off
;	 CONT(            8,-1,15,s_launch2)
;	STSND(snd_j2b,repl,8,-1,08,s_passby)	;Passby
;	 CONT(            8,-1,09,s_launch)
	STSND(snd_j3,repl,8,-1,09,s_rblow)		;Reactor Blows Up
	 CONT(            8,-1,10,s_rblow2)
	 CONT(            8,-1,12,s_rblow2)
	 CONT(            8,-1,13,s_rblow2)
	 CONT(            8,-1,11,s_rblow3)
	 CONT(            8,-1,14,s_rblow3)
	 CONT(            8,-1,15,s_rblow3)
;	STSND(snd_j4,repl,8,-1,08,s_escthrust)	;Escape Pod Launch - Thrust
;	 CONT(            8,-1,09,s_escthrust2)
;	STSND(snd_j5,repl,8,-1,08,s_escdie)	;Escape Pod Engine Die
	STSND(snd_j6,repl,8,-1,08,s_escfall)	;Escape Pod Fall
;	STSND(snd_j7,repl,8,-1,08,s_esccrash)	;Escape Pod Crash
	
;*    Music
;*    Mystery - Enter Maze
	STSND(snd_mys,repl,6,-1,00,s_mystery)		
	 CONT(             6,-1,02,s_mystery2)
	 CONT(             6,-1,04,s_mystery3)
	 CONT(             6,-1,12,s_mystery4)
	 
;*    Breakout
	STSND(snd_brk,repl,6,-1,00,s_breakout)		
	 CONT(             6,-1,02,s_breakout2)
	 CONT(             6,-1,14,s_breakout3)
	 CONT(             6,-1,12,s_breakout4)

;* 	Start
      STSND(snd_str,repl,6,-1,00,s_start)
       CONT(             6,-1,02,s_start2)
       CONT(             6,-1,14,s_start3)
       CONT(             6,-1,12,s_start4)
       
;*	Hero Theme - Exit Maze
	STSND(snd_hro,repl,6,-1,00,s_escape)
	 CONT(             6,-1,02,s_escape2)
	 CONT(             6,-1,04,s_escape3)
	 CONT(             6,-1,12,s_escape4)
	 
;* 	High Score
	STSND(snd_hsc,repl,6,-1,00,s_highscore5)
	 CONT(             6,-1,02,s_highscore6)
	 CONT(             6,-1,14,s_highscore7)
	 CONT(             6,-1,12,s_highscore8)
	
;* Stop Exit Maze Music
	KILID(snd_stm,snd_hro)
	
;* Triumph
;	STSND(snd_rex,repl,6,-1,00,s_highscore)		
;	 CONT(             6,-1,02,s_highscore2)
;	 CONT( 		 6,-1,04,s_highscore3)
;      CONT(             6,-1,06,s_highscore4)
	
	SILENT(snd_off)
	NOISY(snd_onn)
	SETATR(snd_med,5)
	SETFL(snd_on3,3)
	CLRFL(snd_of3,3)
	KILPRI(snd_kil5,5)
	KILDEV(snd_stp0,0,0)
	 	

	.org rpm_end
;*******************************************************************
	.sbttl "Tune Data"
;*******************************************************************
;* This is the actual data for each sound
;*******************************************************************

	NEWDATA2(d2_coin,$07)
	SCTRL(20,2)              ;Eff Slope:10     Dur:2    Net:20
	SCTRL(0,8)               ;Eff Slope:0      Dur:8    Net:20
	SCTRL(-4,2)              ;Eff Slope:-2     Dur:2    Net:16
	SCTRL(-2,14)             ;Eff Slope:-1     Dur:14   Net:2
	SCTRL(18,2)              ;Eff Slope:9      Dur:2    Net:20
	SCTRL(0,12)              ;Eff Slope:0      Dur:12   Net:20
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:18
	SCTRL(-4,4)              ;Eff Slope:-2     Dur:4    Net:10
	SCTRL(-2,8)              ;Eff Slope:-1     Dur:8    Net:2
	; .byte $02,$28,$08,$00
	; .byte $02,$F8,$0E,$FC
	; .byte $02,$24,$0C,$00
	; .byte $02,$FC,$04,$F8
	; .byte $08,$FC

	NEWDATA1(d1_coin,$07)
	SFREQ(40,2)              ;Eff Slope:2.5    Dur:2    Net:5
	SFREQ(0,22)              ;Eff Slope:0      Dur:22   Net:5
	SFREQ(16,2)              ;Eff Slope:1      Dur:2    Net:7
	SFREQ(0,28)              ;Eff Slope:0      Dur:28   Net:7

	NEWTUNE(s_coin) 	
	.byte $86,d1_coin
	.byte $87,d2_coin
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$40
	.byte $00,$36
	.byte $00,$00
	
;****************************************
	NEWDATA2(t2_launch,$08)
	SCTRL(4,2)               ;Eff Slope:2      Dur:2    Net:4
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:4
	SCTRL(4,2)               ;Eff Slope:2      Dur:2    Net:8
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:8
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:10
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:10
	SCTRL(2,4)               ;Eff Slope:1      Dur:4    Net:14
	SCTRL(0,26)              ;Eff Slope:0      Dur:26   Net:14
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:16
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:16
	SCTRL(4,2)               ;Eff Slope:2      Dur:2    Net:20
	SCTRL(0,8)               ;Eff Slope:0      Dur:8    Net:20
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:22
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:22
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:24
	SCTRL(0,8)               ;Eff Slope:0      Dur:8    Net:24
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:26
	SCTRL(0,32)              ;Eff Slope:0      Dur:32   Net:26
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:28
	SCTRL(0,26)              ;Eff Slope:0      Dur:26   Net:28
	SCTRL(2,2)               ;Eff Slope:1      Dur:2    Net:30
	SCTRL(0,82)              ;Eff Slope:0      Dur:82   Net:30
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:28
	SCTRL(0,40)              ;Eff Slope:0      Dur:40   Net:28
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:26
	SCTRL(0,18)              ;Eff Slope:0      Dur:18   Net:26
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:24
	SCTRL(0,20)              ;Eff Slope:0      Dur:20   Net:24
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:22
	SCTRL(0,12)              ;Eff Slope:0      Dur:12   Net:22
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:20
	SCTRL(0,12)              ;Eff Slope:0      Dur:12   Net:20
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:18
	SCTRL(0,8)               ;Eff Slope:0      Dur:8    Net:18
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:16
	SCTRL(0,10)              ;Eff Slope:0      Dur:10   Net:16
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:14
	SCTRL(0,8)               ;Eff Slope:0      Dur:8    Net:14
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:12
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:12
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:10
	SCTRL(0,6)               ;Eff Slope:0      Dur:6    Net:10
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:8
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:8
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:6
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:6
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:4
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:4
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:2
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:2
	; .byte $02,$08,$02,$00
	; .byte $02,$08,$04,$00
	; .byte $02,$04,$02,$00
	; .byte $04,$04,$1A,$00
	; .byte $02,$04,$04,$00
	; .byte $02,$08,$08,$00
	; .byte $02,$04,$04,$00
	; .byte $02,$04,$08,$00
	; .byte $02,$04,$20,$00
	; .byte $02,$04,$1A,$00
	; .byte $02,$04,$52,$00
	; .byte $02,$FC,$28,$00
	; .byte $02,$FC,$12,$00
	; .byte $02,$FC,$14,$00
	; .byte $02,$FC,$0C,$00
	; .byte $02,$FC,$0C,$00
	; .byte $02,$FC,$08,$00
	; .byte $02,$FC,$0A,$00
	; .byte $02,$FC,$08,$00
	; .byte $02,$FC,$04,$00
	; .byte $02,$FC,$06,$00
	; .byte $02,$FC,$02,$00
	; .byte $02,$FC,$02,$00
	; .byte $02,$FC,$02,$00
	; .byte $02,$FC,$02,$00
	
	NEWDATA1(t1_launch,$08)
	SFREQ(1832,2)            ;Eff Slope:114.5  Dur:2    Net:229
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:228
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:225
	SFREQ(-8,10)             ;Eff Slope:-0.5   Dur:10   Net:220
	SFREQ(-16,4)             ;Eff Slope:-1     Dur:4    Net:216
	SFREQ(-48,2)             ;Eff Slope:-3     Dur:2    Net:210
	SFREQ(-32,2)             ;Eff Slope:-2     Dur:2    Net:206
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:204
	SFREQ(-32,2)             ;Eff Slope:-2     Dur:2    Net:200
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:198
	SFREQ(-32,4)             ;Eff Slope:-2     Dur:4    Net:190
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:188
	SFREQ(-40,2)             ;Eff Slope:-2.5   Dur:2    Net:183
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:180
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:179
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:177
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:175
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:173
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:171
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:169
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:166
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:165
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:165
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:162
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:161
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:159
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:156
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:154
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:152
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:151
	SFREQ(-16,4)             ;Eff Slope:-1     Dur:4    Net:147
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:147
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:145
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:142
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:141
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:139
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:137
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:135
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:132
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:132
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:131
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:128
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:127
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:125
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:123
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:121
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:118
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:117
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:115
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:113
	SFREQ(-16,4)             ;Eff Slope:-1     Dur:4    Net:109
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:109
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:107
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:104
	SFREQ(-16,4)             ;Eff Slope:-1     Dur:4    Net:100
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:98
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:96
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:93
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:92
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:90
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:88
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:85
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:83
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:81
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:79
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:78
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:76
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:76
	SFREQ(-16,4)             ;Eff Slope:-1     Dur:4    Net:72
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:71
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:68
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:67
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:67
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:65
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:64
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:61
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:59
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:59
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:57
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:54
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:52
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:52
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:50
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:49
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:49
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:47
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:46
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:46
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:45
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:43
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:43
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:41
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:40
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:40
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:38
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:37
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:37
	SFREQ(-8,6)              ;Eff Slope:-0.5   Dur:6    Net:34
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:34
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:31
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:31
	SFREQ(-8,4)              ;Eff Slope:-0.5   Dur:4    Net:29
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:29
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:28
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:28
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:27
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:27
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:26
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:26
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:25
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:25
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:24
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:24
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:23
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:23
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:22
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:22
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:21
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:21
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:20
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:20
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:19
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:19
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:18
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:18
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:17
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:17
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:16
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:16
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:15
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:15
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:14
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:14
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:13
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:13
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:12
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:12
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:11
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:11
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:10
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:10
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:9
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:9
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:8
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:8
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:7
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:7
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:6
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:6
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:5
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:5
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:4
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:4
	; .byte $02,$50,$0E,$02
	; .byte $F0,$FF,$02
	; .byte $D0,$FF,$0A
	; .byte $F0,$FF,$04
	; .byte $E0,$FF,$02
	; .byte $A0,$FF,$02
	; .byte $C0,$FF,$02
	; .byte $E0,$FF,$02
	; .byte $C0,$FF,$02
	; .byte $E0,$FF,$04
	; .byte $C0,$FF,$02
	; .byte $E0,$FF,$02
	; .byte $B0,$FF,$02
	; .byte $D0,$FF,$02
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$04
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$04
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$02
	; .byte $D0,$FF,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $D0,$FF,$02
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$02
	; .byte $D0,$FF,$04
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$02
	; .byte $F0,$FF,$04
	; .byte $E0,$FF,$02
	; .byte $00,$00,$02
	; .byte $E0,$FF,$02
	; .byte $D0,$FF,$02
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$04
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$02
	; .byte $D0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $D0,$FF,$02
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$04
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$02
	; .byte $D0,$FF,$02
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$04
	; .byte $F0,$FF,$04
	; .byte $E0,$FF,$02
	; .byte $00,$00,$02
	; .byte $E0,$FF,$02
	; .byte $D0,$FF,$04
	; .byte $E0,$FF,$04
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$02
	; .byte $D0,$FF,$02
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$04
	; .byte $F0,$FF,$02
	; .byte $D0,$FF,$02
	; .byte $E0,$FF,$04
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$02
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$04
	; .byte $00,$00,$04
	; .byte $E0,$FF,$02
	; .byte $F0,$FF,$02
	; .byte $D0,$FF,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $E0,$FF,$02
	; .byte $F0,$FF,$02
	; .byte $D0,$FF,$02
	; .byte $E0,$FF,$04
	; .byte $00,$00,$04
	; .byte $F0,$FF,$02
	; .byte $D0,$FF,$02
	; .byte $E0,$FF,$04
	; .byte $00,$00,$02
	; .byte $E0,$FF,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $E0,$FF,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $E0,$FF,$04
	; .byte $00,$00,$02
	; .byte $E0,$FF,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $E0,$FF,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$06
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $D0,$FF,$04
	; .byte $00,$00,$04
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00,$02
	; .byte $F0,$FF,$02
	; .byte $00,$00,$02
	; .byte $F0,$FF,$04
	; .byte $00,$00
	
	NEWTUNE(s_launch)
	.byte $86,t1_launch
	.byte $87,t2_launch
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$80
	.byte $00,$FE
	.byte $8A,$80
	.byte $00,$8F
	.byte $00,$00
	
;**********************************
	NEWDATA2(t2_launch2,$09)
	SCTRL(30,2)              ;Eff Slope:15     Dur:2    Net:30
	SCTRL(0,34)              ;Eff Slope:0      Dur:34   Net:30
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:28
	SCTRL(0,18)              ;Eff Slope:0      Dur:18   Net:28
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:26
	SCTRL(0,20)              ;Eff Slope:0      Dur:20   Net:26
	SCTRL(-2,4)              ;Eff Slope:-1     Dur:4    Net:22
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:22
	SCTRL(-2,4)              ;Eff Slope:-1     Dur:4    Net:18
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:18
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:16
	SCTRL(-4,2)              ;Eff Slope:-2     Dur:2    Net:12
	SCTRL(0,6)               ;Eff Slope:0      Dur:6    Net:12
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:10
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:10
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:8
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:8
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:6
	SCTRL(0,6)               ;Eff Slope:0      Dur:6    Net:6
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:4
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:4
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:2
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:2
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:0
	SCTRL(0,254)             ;Eff Slope:0      Dur:254  Net:0
	SCTRL(0,8)               ;Eff Slope:0      Dur:8    Net:0
	; .byte $02,$3C,$22,$00
	; .byte $02,$FC,$12,$00
	; .byte $02,$FC,$14,$00
	; .byte $04,$FC,$02,$00
	; .byte $04,$FC,$02,$00
	; .byte $02,$FC,$02,$F8
	; .byte $06,$00,$02,$FC
	; .byte $04,$00,$02,$FC
	; .byte $04,$00,$02,$FC
	; .byte $06,$00,$02,$FC
	; .byte $04,$00,$02,$FC
	; .byte $04,$00,$02,$FC
	; .byte $FE,$00,$08,$00
	
	NEWDATA1(t1_launch2,$09)
	SFREQ(720,2)             ;Eff Slope:45     Dur:2    Net:90
	SFREQ(0,254)             ;Eff Slope:0      Dur:254  Net:90
	SFREQ(0,140)             ;Eff Slope:0      Dur:140  Net:90
	
	NEWTUNE(s_launch2)
	.byte $86,t1_launch2
	.byte $87,t2_launch2
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$00
	.byte $00,$FE
	.byte $8A,$00
	.byte $00,$8F
	.byte $00,$00
	
;****************************************
	NEWDATA2(t2_passby,$0a)
	SCTRL(2,4)               ;Eff Slope:1      Dur:4    Net:4
	SCTRL(4,2)               ;Eff Slope:2      Dur:2    Net:8
	SCTRL(6,2)               ;Eff Slope:3      Dur:2    Net:14
	SCTRL(8,4)               ;Eff Slope:4      Dur:4    Net:30
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:30
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:28
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:28
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:26
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:26
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:24
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:24
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:22
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:22
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:20
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:20
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:18
	SCTRL(0,4)               ;Eff Slope:0      Dur:4    Net:18
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:16
	SBEGIN
		SCTRL(0,6)               ;Eff Slope:0      Dur:6    Net:16
		SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:14
	SLOOP(6)                ;Loop Back 6 times.
	SCTRL(0,26)              ;Eff Slope:0      Dur:26   Net:14
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:12
	SCTRL(0,152)             ;Eff Slope:0      Dur:152  Net:12
	
	; .byte $04,$04,$02,$08,$02,$0C,$04,$10
	; .byte $02,$00,$02,$FC,$02,$00,$02,$FC,$04,$00,$02,$FC,$02,$00,$02,$FC
	; .byte $02,$00,$02,$FC,$04,$00,$02,$FC,$04,$00,$02,$FC,$06,$00,$02,$FC
	; .byte $FF,$06,$07,$1A,$00,$02,$FC,$98,$00
	
	NEWDATA1(t1_passby,$0a)
	SFREQ(112,2)             ;Eff Slope:7      Dur:2    Net:14
	SFREQ(-8,10)             ;Eff Slope:-0.5   Dur:10   Net:9
	SFREQ(8,254)             ;Eff Slope:0.5    Dur:254  Net:136
	SFREQ(8,12)              ;Eff Slope:0.5    Dur:12   Net:142
	SFREQ(-1136,2)           ;Eff Slope:-71    Dur:2    Net:0
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:0
	
	NEWTUNE(s_passby)	
	.byte $86,t1_passby
	.byte $87,t2_passby
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$80
	.byte $00,$FE
	.byte $8A,$80
	.byte $00,$03
	.byte $8A,$00
	.byte $00,$1B
	.byte $00,$00
	
;****************************************
	NEWDATA2(t2_passby2,$0b)
	SCTRL(2,4)               ;Eff Slope:1      Dur:4    Net:4
	SCTRL(4,2)               ;Eff Slope:2      Dur:2    Net:8
	SCTRL(6,2)               ;Eff Slope:3      Dur:2    Net:14
	SCTRL(8,4)               ;Eff Slope:4      Dur:4    Net:30
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:30
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:28
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:28
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:26
	SBEGIN
		SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:26
		SCTRL(-2,4)              ;Eff Slope:-1     Dur:4    Net:22
	SLOOP(2)                 ;Loop Back 2 times.
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:22
	SCTRL(-2,10)             ;Eff Slope:-1     Dur:10   Net:12
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:12
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:10
	SCTRL(0,2)               ;Eff Slope:0      Dur:2    Net:10
	SCTRL(-2,2)              ;Eff Slope:-1     Dur:2    Net:8
	SCTRL(0,248)             ;Eff Slope:0      Dur:248  Net:8
	; .byte $04,$04,$02,$08,$02,$0C,$04,$10,$02,$00,$02,$FC,$02,$00,$02
	; .byte $FC,$02,$00,$04,$FC,$FF,$02,$07,$02,$00,$0A,$FC,$02,$00,$02,$FC
	; .byte $02,$00,$02,$FC,$F8,$00
	
	NEWDATA1(t1_passby2,$0b)
	SFREQ(208,2)             ;Eff Slope:13     Dur:2    Net:26
	SFREQ(-8,10)             ;Eff Slope:-0.5   Dur:10   Net:21
	SFREQ(8,254)             ;Eff Slope:0.5    Dur:254  Net:148
	SFREQ(8,2)               ;Eff Slope:0.5    Dur:2    Net:149
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:146
	SFREQ(8,10)              ;Eff Slope:0.5    Dur:10   Net:151
	SFREQ(16,2)              ;Eff Slope:1      Dur:2    Net:153
	SFREQ(-1224,2)           ;Eff Slope:-76.5  Dur:2    Net:0
	SFREQ(0,22)              ;Eff Slope:0      Dur:22   Net:0
	; .byte $02,$A0,$01,$0A,$F0,$FF,$FE,$10,$00,$02
	; .byte $10,$00,$02,$D0,$FF,$0A,$10,$00,$02,$20,$00,$02,$70,$F6,$16,$00
	; .byte $00
	
	NEWTUNE(s_passby2)
	.byte $86,t1_passby2
	.byte $87,t2_passby2
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$C0
	.byte $00,$FE
	.byte $8A,$C0
	.byte $00,$1F
	.byte $8A,$00
	.byte $00,$03
	.byte $8A,$80
	.byte $00,$13
	.byte $00,$00
	
;***************************************
	NEWDATA2(t2_galert,$0c)
	.byte $02,$0C,$FE,$00
	
	NEWDATA1(t1_galert,$0c)
	SFREQ(920,2)             ;Eff Slope:57.5   Dur:2    Net:115
	SFREQ(8,2)               ;Eff Slope:0.5    Dur:2    Net:116
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:116
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:84
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:84
	SFREQ(144,2)             ;Eff Slope:9      Dur:2    Net:102
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:102
	SFREQ(-208,2)            ;Eff Slope:-13    Dur:2    Net:76
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:76
	SFREQ(304,2)             ;Eff Slope:19     Dur:2    Net:114
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:114
	SFREQ(-240,2)            ;Eff Slope:-15    Dur:2    Net:84
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:84
	SFREQ(352,2)             ;Eff Slope:22     Dur:2    Net:128
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:128
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:96
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:96
	SFREQ(128,2)             ;Eff Slope:8      Dur:2    Net:112
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:112
	SFREQ(-232,2)            ;Eff Slope:-14.5  Dur:2    Net:83
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:83
	SFREQ(16,2)              ;Eff Slope:1      Dur:2    Net:85
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:85
	SFREQ(-168,2)            ;Eff Slope:-10.5  Dur:2    Net:64
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:64
	SFREQ(640,2)             ;Eff Slope:40     Dur:2    Net:144
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:144
	SFREQ(-288,2)            ;Eff Slope:-18    Dur:2    Net:108
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:108
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:106
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:106
	SFREQ(-216,2)            ;Eff Slope:-13.5  Dur:2    Net:79
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:79
	SFREQ(336,2)             ;Eff Slope:21     Dur:2    Net:121
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:121
	SFREQ(-240,2)            ;Eff Slope:-15    Dur:2    Net:91
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:91
	SFREQ(424,2)             ;Eff Slope:26.5   Dur:2    Net:144
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:144
	SFREQ(-288,2)            ;Eff Slope:-18    Dur:2    Net:108
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:108
	SFREQ(520,2)             ;Eff Slope:32.5   Dur:2    Net:173
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:173
	SFREQ(-360,2)            ;Eff Slope:-22.5  Dur:2    Net:128
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:128
	SFREQ(200,2)             ;Eff Slope:12.5   Dur:2    Net:153
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:153
	SFREQ(-312,2)            ;Eff Slope:-19.5  Dur:2    Net:114
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:114
	SFREQ(-144,2)            ;Eff Slope:-9     Dur:2    Net:96
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:96
	SFREQ(-192,2)            ;Eff Slope:-12    Dur:2    Net:72
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:72
	SFREQ(224,2)             ;Eff Slope:14     Dur:2    Net:100
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:100
	SFREQ(-200,2)            ;Eff Slope:-12.5  Dur:2    Net:75
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:75
	SFREQ(232,2)             ;Eff Slope:14.5   Dur:2    Net:104
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:104
	SFREQ(-208,2)            ;Eff Slope:-13    Dur:2    Net:78
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:78
	SFREQ(240,2)             ;Eff Slope:15     Dur:2    Net:108
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:108
	SFREQ(-216,2)            ;Eff Slope:-13.5  Dur:2    Net:81
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:81
	SFREQ(264,2)             ;Eff Slope:16.5   Dur:2    Net:114
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:114
	SFREQ(-232,2)            ;Eff Slope:-14.5  Dur:2    Net:85
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:85
	SFREQ(288,2)             ;Eff Slope:18     Dur:2    Net:121
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:121
	SFREQ(-240,2)            ;Eff Slope:-15    Dur:2    Net:91
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:91
	SFREQ(296,2)             ;Eff Slope:18.5   Dur:2    Net:128
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:128
	SFREQ(-256,2)            ;Eff Slope:-16    Dur:2    Net:96
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:96
	SFREQ(320,2)             ;Eff Slope:20     Dur:2    Net:136
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:136
	SFREQ(-272,2)            ;Eff Slope:-17    Dur:2    Net:102
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:102
	SFREQ(336,2)             ;Eff Slope:21     Dur:2    Net:144
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:144
	SFREQ(-288,2)            ;Eff Slope:-18    Dur:2    Net:108
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:108
	SFREQ(360,2)             ;Eff Slope:22.5   Dur:2    Net:153
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:153
	; .byte $02,$30,$07,$02,$10,$00,$02,$00,$00,$02,$00,$FE,$04,$00,$00,$02,$20
	; .byte $01,$04,$00,$00,$02,$60,$FE,$04,$00,$00,$02,$60,$02,$04,$00,$00
	; .byte $02,$20,$FE,$04,$00,$00,$02,$C0,$02,$04,$00,$00,$02,$00,$FE,$04
	; .byte $00,$00,$02,$00,$01,$04,$00,$00,$02,$30,$FE,$04,$00,$00,$02,$20
	; .byte $00,$04,$00,$00,$02,$B0,$FE,$04,$00,$00,$02,$00,$05,$04,$00,$00
	; .byte $02,$C0,$FD,$04,$00,$00,$02,$E0,$FF,$04,$00,$00,$02,$50,$FE,$04
	; .byte $00,$00,$02,$A0,$02,$04,$00,$00,$02,$20,$FE,$04,$00,$00,$02,$50
	; .byte $03,$04,$00,$00,$02,$C0,$FD,$04,$00,$00,$02,$10,$04,$04,$00,$00
	; .byte $02,$30,$FD,$04,$00,$00,$02,$90,$01,$04,$00,$00,$02,$90,$FD,$04
	; .byte $00,$00,$02,$E0,$FE,$04,$00,$00,$02,$80,$FE,$04,$00,$00,$02,$C0
	; .byte $01,$04,$00,$00,$02,$70,$FE,$04,$00,$00,$02,$D0,$01,$04,$00,$00
	; .byte $02,$60,$FE,$04,$00,$00,$02,$E0,$01,$04,$00,$00,$02,$50,$FE,$04
	; .byte $00,$00,$02,$10,$02,$04,$00,$00,$02,$30,$FE,$04,$00,$00,$02,$40
	; .byte $02,$04,$00,$00,$02,$20,$FE,$04,$00,$00,$02,$50,$02,$04,$00,$00
	; .byte $02,$00,$FE,$04,$00,$00,$02,$80,$02,$04,$00,$00,$02,$E0,$FD,$04
	; .byte $00,$00,$02,$A0,$02,$04,$00,$00,$02,$C0,$FD,$04,$00,$00,$02,$D0
	; .byte $02,$04,$00,$00
	
	NEWTUNE(s_galert)
	.byte $86,t1_galert
	.byte $87,t2_galert
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$FE
	.byte $00,$00
	
;*************************************
	NEWDATA2(t2_alert1,$0d)
	NEWDATA1(t1_alert1,$0d)
	NEWTUNE(s_alert1)
	.byte $00,$00

;*************************************	
	NEWDATA2(t2_blaunch,$0e)
	.byte $02,$3C,$04,$00
	
	NEWDATA1(t1_blaunch,$0e)
	SFREQ(648,2)             ;Eff Slope:40.5   Dur:2    Net:81
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:81
	; .byte $02,$10
	; .byte $05,$04
	; .byte $00,$00

	NEWTUNE(s_blaunch)	
	.byte $86,t1_blaunch
	.byte $87,t2_blaunch
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$06
	.byte $00,$00
	
;*************************************
	NEWDATA2(t2_bbrick,$0f)
	.byte $02,$3C,$04,$00
	
	NEWDATA1(t1_bbrick,$0f)
	SFREQ(448,2)             ;Eff Slope:28     Dur:2    Net:56
	SFREQ(8,2)               ;Eff Slope:0.5    Dur:2    Net:57
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:57
	; .byte $02,$80,$03,$02
	; .byte $10,$00,$02,$00
	; .byte $00
	
	NEWTUNE(s_bbrick)	
	.byte $86,t1_bbrick
	.byte $87,t2_bbrick
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$06
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_bpaddle,$10)
	.byte $02,$3C,$04,$00
	
	NEWDATA1(t1_bpaddle,$10)
	SFREQ(968,2)             ;Eff Slope:60.5   Dur:2    Net:121
	SFREQ(0,4)               ;Eff Slope:0      Dur:4    Net:121
	; .byte $02,$90,$07,$04
	; .byte $00,$00
	
	NEWTUNE(s_bpaddle)	
	.byte $86,t1_bpaddle
	.byte $87,t2_bpaddle
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$06
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_bmissp,$11)
	.byte $02,$3C,$22,$00
	
	NEWDATA1(t1_bmissp,$11)
	SFREQ(1824,2)            ;Eff Slope:114    Dur:2    Net:228
	SBEGIN
		SFREQ(24,2)              ;Eff Slope:1.5    Dur:2    Net:231
		SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:228
	SLOOP(4)                 ;Loop Back 4 times.
	SFREQ(24,2)              ;Eff Slope:1.5    Dur:2    Net:231
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:230
	SFREQ(32,2)              ;Eff Slope:2      Dur:2    Net:234
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:232
	SFREQ(40,2)              ;Eff Slope:2.5    Dur:2    Net:237
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:234
	SFREQ(48,2)              ;Eff Slope:3      Dur:2    Net:240
	; .byte $02,$40,$0E
	; .byte $02,$30,$00
	; .byte $02,$D0,$FF
	; .byte $FF,$04,$09
	; .byte $02,$30,$00
	; .byte $02,$F0,$FF
	; .byte $02,$40,$00
	; .byte $02,$E0,$FF
	; .byte $02,$50,$00
	; .byte $02,$D0,$FF
	; .byte $02,$60,$00
	
	NEWTUNE(s_bmissp)	
	.byte $86,t1_bmissp
	.byte $87,t2_bmissp
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$24
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_digit,$12)
	.byte $02,$10,$02,$18
	.byte $02,$14,$0E,$00
	.byte $06,$F0,$02,$F4
	
	NEWDATA1(t1_digit,$12)
	SFREQ(608,2)             ;Eff Slope:38     Dur:2    Net:76
	SFREQ(-32,2)             ;Eff Slope:-2     Dur:2    Net:72
	SFREQ(-88,2)             ;Eff Slope:-5.5   Dur:2    Net:61
	SFREQ(-32,2)             ;Eff Slope:-2     Dur:2    Net:57
	SFREQ(-48,2)             ;Eff Slope:-3     Dur:2    Net:51
	SFREQ(-24,4)             ;Eff Slope:-1.5   Dur:4    Net:45
	SFREQ(-16,2)             ;Eff Slope:-1     Dur:2    Net:43
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:40
	SFREQ(-32,2)             ;Eff Slope:-2     Dur:2    Net:36
	SFREQ(0,8)               ;Eff Slope:0      Dur:8    Net:36
	; .byte $02,$C0,$04,$02,$C0,$FF,$02,$50,$FF,$02,$C0,$FF,$02,$A0,$FF,$04,$D0,$FF
	; .byte $02,$E0,$FF,$02,$D0,$FF,$02,$C0,$FF,$08,$00,$00

	NEWTUNE(s_digit)	
	.byte	$86,t1_digit
	.byte $87,t2_digit
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$C0
	.byte $00,$1C
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_notused1,$13)
	NEWDATA1(t1_notused1,$13)
	NEWTUNE(s_notused1)	
	.byte $00,$00

;**************************************
	NEWDATA2(t2_notused2,$14)
	NEWDATA1(t1_notused2,$14)
	NEWTUNE(s_notused2)	
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_notused3,$15)
	NEWDATA1(t1_notused3,$15)
	NEWTUNE(s_notused3)	
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_ccorrect,$16)
	.byte $02,$20,$12,$00,$02,$F8,$06,$00,$02,$FC,$02,$00,$FF,$02,$07,$04
	.byte $FC,$3C,$00
	
	NEWDATA1(t1_ccorrect,$16)
	SFREQ(400,2)             ;Eff Slope:25     Dur:2    Net:50
	SFREQ(120,2)             ;Eff Slope:7.5    Dur:2    Net:65
	SFREQ(-200,2)            ;Eff Slope:-12.5  Dur:2    Net:40
	SFREQ(128,2)             ;Eff Slope:8      Dur:2    Net:56
	SFREQ(-184,2)            ;Eff Slope:-11.5  Dur:2    Net:33
	SFREQ(120,2)             ;Eff Slope:7.5    Dur:2    Net:48
	SFREQ(-176,2)            ;Eff Slope:-11    Dur:2    Net:26
	SFREQ(120,2)             ;Eff Slope:7.5    Dur:2    Net:41
	SFREQ(-160,2)            ;Eff Slope:-10    Dur:2    Net:21
	SFREQ(112,2)             ;Eff Slope:7      Dur:2    Net:35
	SFREQ(-136,2)            ;Eff Slope:-8.5   Dur:2    Net:18
	SFREQ(104,2)             ;Eff Slope:6.5    Dur:2    Net:31
	SFREQ(-120,2)            ;Eff Slope:-7.5   Dur:2    Net:16
	SFREQ(96,2)              ;Eff Slope:6      Dur:2    Net:28
	SBEGIN
		SFREQ(-112,2)            ;Eff Slope:-7     Dur:2    Net:14
		SFREQ(88,2)              ;Eff Slope:5.5    Dur:2    Net:25
	SLOOP(2)                 ;Loop Back 2 times.
	SBEGIN
		SFREQ(-96,2)             ;Eff Slope:-6     Dur:2    Net:13
		SFREQ(88,2)              ;Eff Slope:5.5    Dur:2    Net:24
	SLOOP(4)                 ;Loop Back 4 times.
	SFREQ(-88,2)             ;Eff Slope:-5.5   Dur:2    Net:13
	SFREQ(80,2)              ;Eff Slope:5      Dur:2    Net:23
	SFREQ(-88,2)             ;Eff Slope:-5.5   Dur:2    Net:12
	SFREQ(80,2)              ;Eff Slope:5      Dur:2    Net:22
	SFREQ(-80,2)             ;Eff Slope:-5     Dur:2    Net:12
	SFREQ(72,2)              ;Eff Slope:4.5    Dur:2    Net:21
	SFREQ(-80,2)             ;Eff Slope:-5     Dur:2    Net:11
	SFREQ(72,2)              ;Eff Slope:4.5    Dur:2    Net:20
	SFREQ(-72,2)             ;Eff Slope:-4.5   Dur:2    Net:11
	SFREQ(64,2)              ;Eff Slope:4      Dur:2    Net:19
	SFREQ(-72,2)             ;Eff Slope:-4.5   Dur:2    Net:10
	SFREQ(64,2)              ;Eff Slope:4      Dur:2    Net:18
	SFREQ(-64,2)             ;Eff Slope:-4     Dur:2    Net:10
	SFREQ(56,2)              ;Eff Slope:3.5    Dur:2    Net:17
	SFREQ(-64,2)             ;Eff Slope:-4     Dur:2    Net:9
	SFREQ(56,2)              ;Eff Slope:3.5    Dur:2    Net:16
	SFREQ(-56,2)             ;Eff Slope:-3.5   Dur:2    Net:9
	SFREQ(48,2)              ;Eff Slope:3      Dur:2    Net:15
	SFREQ(-56,2)             ;Eff Slope:-3.5   Dur:2    Net:8
	SFREQ(48,2)              ;Eff Slope:3      Dur:2    Net:14
	SFREQ(-48,2)             ;Eff Slope:-3     Dur:2    Net:8
	SFREQ(48,2)              ;Eff Slope:3      Dur:2    Net:14
	; .byte $02,$20,$03
	; .byte $02,$F0,$00
	; .byte $02,$70,$FE
	; .byte $02,$00,$01
	; .byte $02,$90,$FE
	; .byte $02,$F0,$00
	; .byte $02,$A0,$FE
	; .byte $02,$F0,$00
	; .byte $02,$C0,$FE
	; .byte $02,$E0,$00
	; .byte $02,$F0,$FE
	; .byte $02,$D0,$00
	; .byte $02,$10,$FF
	; .byte $02,$C0,$00
	; .byte $02,$20,$FF
	; .byte $02,$B0,$00
	; .byte $FF,$02,$09
	; .byte $02,$40,$FF
	; .byte $02,$B0,$00
	; .byte $FF,$04,$09
	; .byte $02,$50,$FF
	; .byte $02,$A0,$00
	; .byte $02,$50,$FF
	; .byte $02,$A0,$00
	; .byte $02,$60,$FF
	; .byte $02,$90,$00
	; .byte $02,$60,$FF
	; .byte $02,$90,$00
	; .byte $02,$70,$FF
	; .byte $02,$80,$00
	; .byte $02,$70,$FF
	; .byte $02,$80,$00
	; .byte $02,$80,$FF
	; .byte $02,$70,$00
	; .byte $02,$80,$FF
	; .byte $02,$70,$00
	; .byte $02,$90,$FF
	; .byte $02,$60,$00
	; .byte $02,$90,$FF
	; .byte $02,$60,$00
	; .byte $02,$A0,$FF
	; .byte $02,$60,$00
	
	NEWTUNE(s_ccorrect)	
	.byte $86,t1_ccorrect
	.byte $87,t2_ccorrect
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$68
	.byte $00,$00

;**************************************
	NEWDATA2(t2_exp,$17)
	.byte $02,$3C,$FE,$00,$FE,$00
	
	NEWDATA1(t1_exp,$17)
	.byte $04,$00,$00,$FE,$10,$00,$FE,$10,$00
	
	NEWTUNE(s_exp)	
	.byte $86,t1_exp
	.byte $87,t2_exp
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$40
	.byte $00,$FE
	.byte $8A,$40
	.byte $00,$FF
	.byte $8A,$40
	.byte $00,$03
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_exp2,$18)
	.byte $02,$1C,$10,$04,$28,$00,$02,$FC,$0A,$00,$02,$F8
	.byte $08,$00,$02,$FC,$FF,$08,$07,$06,$00,$02,$FC,$04,$00,$02,$FC,$02
	.byte $00,$02,$FC
	
	NEWDATA1(t1_exp2,$18)
	SFREQ(48,2)              ;Eff Slope:3      Dur:2    Net:6
	SFREQ(-48,2)             ;Eff Slope:-3     Dur:2    Net:0
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:0
	SFREQ(80,2)              ;Eff Slope:5      Dur:2    Net:10
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:10
	SFREQ(-80,2)             ;Eff Slope:-5     Dur:2    Net:0
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:0
	SFREQ(72,2)              ;Eff Slope:4.5    Dur:2    Net:9
	SFREQ(-32,2)             ;Eff Slope:-2     Dur:2    Net:5
	SFREQ(8,2)               ;Eff Slope:0.5    Dur:2    Net:6
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:3
	SFREQ(24,2)              ;Eff Slope:1.5    Dur:2    Net:6
	SFREQ(32,2)              ;Eff Slope:2      Dur:2    Net:10
	SFREQ(-32,2)             ;Eff Slope:-2     Dur:2    Net:6
	SFREQ(80,2)              ;Eff Slope:5      Dur:2    Net:16
	SFREQ(-104,2)            ;Eff Slope:-6.5   Dur:2    Net:3
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:3
	SFREQ(152,2)             ;Eff Slope:9.5    Dur:2    Net:22
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:22
	SFREQ(-80,2)             ;Eff Slope:-5     Dur:2    Net:12
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:12
	SFREQ(64,2)              ;Eff Slope:4      Dur:2    Net:20
	SFREQ(-56,2)             ;Eff Slope:-3.5   Dur:2    Net:13
	SFREQ(40,2)              ;Eff Slope:2.5    Dur:2    Net:18
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:17
	SFREQ(-40,2)             ;Eff Slope:-2.5   Dur:2    Net:12
	SFREQ(64,4)              ;Eff Slope:4      Dur:4    Net:28
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:28
	SFREQ(-64,2)             ;Eff Slope:-4     Dur:2    Net:20
	SFREQ(-24,2)             ;Eff Slope:-1.5   Dur:2    Net:17
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:17
	SFREQ(88,2)              ;Eff Slope:5.5    Dur:2    Net:28
	SFREQ(-64,2)             ;Eff Slope:-4     Dur:2    Net:20
	SFREQ(256,2)             ;Eff Slope:16     Dur:2    Net:52
	SFREQ(-200,2)            ;Eff Slope:-12.5  Dur:2    Net:27
	SFREQ(128,2)             ;Eff Slope:8      Dur:2    Net:43
	SFREQ(-128,2)            ;Eff Slope:-8     Dur:2    Net:27
	SFREQ(72,2)              ;Eff Slope:4.5    Dur:2    Net:36
	SFREQ(200,2)             ;Eff Slope:12.5   Dur:2    Net:61
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:61
	SFREQ(-112,2)            ;Eff Slope:-7     Dur:2    Net:47
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:47
	SFREQ(216,2)             ;Eff Slope:13.5   Dur:2    Net:74
	SFREQ(-120,2)            ;Eff Slope:-7.5   Dur:2    Net:59
	SFREQ(-112,2)            ;Eff Slope:-7     Dur:2    Net:45
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:45
	SFREQ(304,2)             ;Eff Slope:19     Dur:2    Net:83
	SFREQ(-144,2)            ;Eff Slope:-9     Dur:2    Net:65
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:65
	SFREQ(-80,2)             ;Eff Slope:-5     Dur:2    Net:55
	SFREQ(176,2)             ;Eff Slope:11     Dur:2    Net:77
	SFREQ(-232,2)            ;Eff Slope:-14.5  Dur:2    Net:48
	SFREQ(376,2)             ;Eff Slope:23.5   Dur:2    Net:95
	SFREQ(-136,2)            ;Eff Slope:-8.5   Dur:2    Net:78
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:78
	SFREQ(-96,2)             ;Eff Slope:-6     Dur:2    Net:66
	SFREQ(344,2)             ;Eff Slope:21.5   Dur:2    Net:109
	SFREQ(-376,2)            ;Eff Slope:-23.5  Dur:2    Net:62
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:62
	SFREQ(160,2)             ;Eff Slope:10     Dur:2    Net:82
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:82
	SFREQ(-88,2)             ;Eff Slope:-5.5   Dur:2    Net:71
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:71
	SFREQ(232,2)             ;Eff Slope:14.5   Dur:2    Net:100
	SFREQ(-88,2)             ;Eff Slope:-5.5   Dur:2    Net:89
	SFREQ(224,2)             ;Eff Slope:14     Dur:2    Net:117
	SFREQ(-328,2)            ;Eff Slope:-20.5  Dur:2    Net:76
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:76
	SFREQ(160,2)             ;Eff Slope:10     Dur:2    Net:96
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:96
	SFREQ(-64,2)             ;Eff Slope:-4     Dur:2    Net:88
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:88
	SFREQ(184,2)             ;Eff Slope:11.5   Dur:2    Net:111
	SFREQ(-144,2)            ;Eff Slope:-9     Dur:2    Net:93
	SFREQ(88,2)              ;Eff Slope:5.5    Dur:2    Net:104
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:104
	SFREQ(120,2)             ;Eff Slope:7.5    Dur:2    Net:119
	SFREQ(-304,2)            ;Eff Slope:-19    Dur:2    Net:81
	SFREQ(160,2)             ;Eff Slope:10     Dur:2    Net:101
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:101
	SFREQ(-72,2)             ;Eff Slope:-4.5   Dur:2    Net:92
	SFREQ(152,2)             ;Eff Slope:9.5    Dur:2    Net:111
	SFREQ(88,2)              ;Eff Slope:5.5    Dur:2    Net:122
	SFREQ(-280,2)            ;Eff Slope:-17.5  Dur:2    Net:87
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:87
	SFREQ(136,2)             ;Eff Slope:8.5    Dur:2    Net:104
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:104
	SFREQ(232,2)             ;Eff Slope:14.5   Dur:2    Net:133
	SFREQ(0,2)               ;Eff Slope:0      Dur:2    Net:133
	; .byte $02,$60,$00,$02,$A0,$FF,$02,$00,$00,$02,$A0,$00,$02
	; .byte $00,$00,$02,$60,$FF,$02,$00,$00,$02,$90,$00,$02,$C0,$FF,$02,$10
	; .byte $00,$02,$D0,$FF,$02,$30,$00,$02,$40,$00,$02,$C0,$FF,$02,$A0,$00
	; .byte $02,$30,$FF,$02,$00,$00,$02,$30,$01,$02,$00,$00,$02,$60,$FF,$02
	; .byte $00,$00,$02,$80,$00,$02,$90,$FF,$02,$50,$00,$02,$F0,$FF,$02,$B0
	; .byte $FF,$04,$80,$00,$02,$00,$00,$02,$80,$FF,$02,$D0,$FF,$02,$00,$00
	; .byte $02,$B0,$00,$02,$80,$FF,$02,$00,$02,$02,$70,$FE,$02,$00,$01,$02
	; .byte $00,$FF,$02,$90,$00,$02,$90,$01,$02,$00,$00,$02,$20,$FF,$02,$00
	; .byte $00,$02,$B0,$01,$02,$10,$FF,$02,$20,$FF,$02,$00,$00,$02,$60,$02
	; .byte $02,$E0,$FE,$02,$00,$00,$02,$60,$FF,$02,$60,$01,$02,$30,$FE,$02
	; .byte $F0,$02,$02,$F0,$FE,$02,$00,$00,$02,$40,$FF,$02,$B0,$02,$02,$10
	; .byte $FD,$02,$00,$00,$02,$40,$01,$02,$00,$00,$02,$50,$FF,$02,$00,$00
	; .byte $02,$D0,$01,$02,$50,$FF,$02,$C0,$01,$02,$70,$FD,$02,$00,$00,$02
	; .byte $40,$01,$02,$00,$00,$02,$80,$FF,$02,$00,$00,$02,$70,$01,$02,$E0
	; .byte $FE,$02,$B0,$00,$02,$00,$00,$02,$F0,$00,$02,$A0,$FD,$02,$40,$01
	; .byte $02,$00,$00,$02,$70,$FF,$02,$30,$01,$02,$B0,$00,$02,$D0,$FD,$02
	; .byte $00,$00,$02,$10,$01,$02,$00,$00,$02,$D0,$01,$02,$00,$00
	
	NEWTUNE(s_exp2)	
	.byte $8B,$80
	.byte $86,t1_exp2
	.byte $87,t2_exp2
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$80
	.byte $00,$B4
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_exp3,$19)
	.byte $02,$3C
	.byte $02,$00,$02,$C4,$04,$00,$02,$3C,$06,$FC,$02,$00,$02,$FC,$FF,$02
	.byte $07,$04,$00,$02,$FC,$FF,$02,$07,$06,$00,$02,$FC,$0C,$00,$02,$FC
	.byte $0A,$00,$02,$FC,$FF,$02,$07,$0E,$00,$02,$FC
	
	NEWDATA1(t1_exp3,$19)
	SFREQ(184,2)             ;Eff Slope:11.5   Dur:2    Net:23
	SFREQ(8,6)               ;Eff Slope:0.5    Dur:6    Net:26
	SBEGIN
		SFREQ(-8,8)              ;Eff Slope:-0.5   Dur:8    Net:22
		SFREQ(8,8)               ;Eff Slope:0.5    Dur:8    Net:26
	SLOOP(6)                ;Loop Back 6 times.
	SFREQ(-8,2)              ;Eff Slope:-0.5   Dur:2    Net:25
	; .byte $02,$70,$01,$06,$10
	; .byte $00,$08,$F0,$FF,$08,$10,$00,$FF,$06,$09,$02,$F0,$FF
	
	NEWTUNE(s_exp3)	
	.byte $86,t1_exp3
	.byte $87,t2_exp3
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$00
	.byte $00,$7A
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_exp4,$1a)
	.byte $02,$3C,$02,$00,$02
	.byte $C4,$04,$00,$02,$3C,$06,$FC,$02,$00,$02,$FC,$02,$00,$02,$FC,$02
	.byte $00,$02,$FC,$04,$00,$FF,$04,$07,$02,$FC,$02,$00,$02,$FC,$02,$00
	.byte $02,$FC,$04,$00,$02,$FC,$04,$00,$02,$FC
	
	NEWDATA1(t1_exp4,$1a)
	.byte $02,$D0,$00,$06,$10,$00
	.byte $08,$F0,$FF,$08,$10,$00,$08,$F0,$FF,$FF,$03,$09
	
	NEWTUNE(s_exp4)	
	.byte $86,t1_exp4
	.byte $87,t2_exp4
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$00
	.byte $00,$50
	.byte $00,$00
;**************************************
	NEWDATA2(t2_exp5,$1b)
	.byte $02,$3C,$0C,$00,$02,$C4
	.byte $06,$00,$02,$3C,$06,$00,$02,$FC,$FF,$03,$07,$04,$00,$02,$FC,$04
	.byte $00,$02,$FC,$02,$00,$02,$FC,$06,$00,$02,$FC,$02,$00,$02,$FC,$02
	.byte $00,$02,$FC,$04,$00,$02,$FC,$06,$00,$02,$FC,$06,$00,$02,$FC,$08
	.byte $00,$02,$FC,$06,$00,$02,$FC
	
	NEWDATA1(t1_exp5,$1b)
	.byte $02,$60,$07,$0A,$F0,$FF,$0C,$10,$00
	.byte $0C,$F0,$FF,$FF,$03,$09,$0C,$10,$00,$08,$F0,$FF
	
	NEWTUNE(s_exp5)	
	.byte $86,t1_exp5
	.byte $87,t2_exp5
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$40
	.byte $00,$80
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_exp6,$1c)
	.byte $02,$3C,$06,$00,$02,$C4
	.byte $06,$00,$02,$3C,$06,$FC,$02,$00,$02,$FC,$FF,$02,$07,$04,$00,$02
	.byte $FC,$FF,$02,$07,$06,$00,$02,$FC,$FF,$02,$07,$08,$00,$02,$FC,$FF
	.byte $02,$07
	
	NEWDATA1(t1_exp6,$1c)
	.byte $02,$C0,$03,$6A,$00,$00
	
	NEWTUNE(s_exp6)	
	.byte $86,t1_exp6
	.byte $87,t2_exp6
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$40
	.byte $00,$6C
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_pfire,$1d)
	.byte $02,$1C,$0A,$00
	
	NEWDATA1(t1_pfire,$1d)
	.byte $02,$60,$01,$02,$70,$00
	.byte $02,$B0,$00,$02,$D0,$00,$02,$30,$01,$02,$80,$01
	
	NEWTUNE(s_pfire)	
	.byte $86,t1_pfire
	.byte $87,t2_pfire
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$0C
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_shotstat,$1e)
	.byte $02,$3C,$04,$00,$06,$F8,$02,$F4,$02,$F8
	
	NEWDATA1(t1_shotstat,$1e)
	.byte $12,$00,$00
	
	NEWTUNE(s_shotstat)
	.byte $86,t1_shotstat
	.byte $87,t2_shotstat
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$00
	.byte $00,$10
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_fishhatch,$1f)
	.byte $02,$20,$12,$00,$02,$F8,$06,$00,$02,$FC,$02
	.byte $00,$02,$FC,$02,$00,$02,$FC,$02,$00,$06,$FC
	
	NEWDATA1(t1_fishhatch,$1f)
	.byte $02,$20,$03,$02,$F0
	.byte $00,$02,$70,$FE,$02,$00,$01,$02,$90,$FE,$02,$F0,$00,$02,$A0,$FE
	.byte $02,$F0,$00,$02,$C0,$FE,$02,$E0,$00,$02,$F0,$FE,$02,$D0,$00,$02
	.byte $10,$FF,$02,$C0,$00,$02,$20,$FF,$02,$B0,$00,$02,$30,$FF,$02,$B0
	.byte $00,$02,$30,$FF,$02,$B0,$00,$02,$40,$FF,$02,$B0,$00,$02,$C0,$FE

	NEWTUNE(s_fishhatch)
	.byte $86,t1_fishhatch
	.byte $87,t2_fishhatch
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$2E
	.byte $00,$00

;**************************************
	NEWDATA2(t2_fishhatch2,$20)
	NEWDATA1(t1_fishhatch2,$20)
	NEWTUNE(s_fishhatch2)
	.byte $00,$00

;**************************************
	NEWDATA2(t2_fishhatch3,$21)
	NEWDATA1(t1_fishhatch3,$21)
	NEWTUNE(s_fishhatch3)
	.byte	$00,$00
	
;**************************************
	NEWDATA2(t2_reactor,$22)
	.byte $02,$28,$04,$00,$02,$FC,$02,$00,$02,$FC,$FF,$05,$07,$06
	.byte $00,$02,$FC,$06,$00,$02,$FC,$20,$00,$02,$04,$06,$00,$02,$04,$06
	.byte $00,$02,$04,$02,$00,$FF,$05,$07,$02,$04,$06,$00
	
	NEWDATA1(t1_reactor,$22)
	.byte $02,$80,$03,$02
	.byte $F0,$FF,$02,$40,$00,$02,$A0,$FF,$02,$80,$00,$02,$A0,$FF,$02,$40
	.byte $00,$04,$E0,$FF,$FF,$07,$15,$02,$40,$00,$02,$A0,$FF,$02,$80,$00
	.byte $02,$A0,$FF,$02,$40,$00,$02,$E0,$FF
	
	NEWTUNE(s_reactor)
	.byte $86,t1_reactor
	.byte $87,t2_reactor
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$80
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_reactor2,$23)
	.byte $02,$04,$04,$00,$02,$04,$02,$00,$FF
	.byte $05,$07,$02,$04,$06,$00,$02,$04,$06,$00,$02,$04,$20,$00,$02,$FC
	.byte $06,$00,$02,$FC,$06,$00,$02,$FC,$02,$00,$FF,$05,$07,$02,$FC,$06,$00
	
	NEWDATA1(t1_reactor2,$23)
	.byte $02,$B0,$01,$02,$E0,$FF,$02,$40,$00,$02,$A0,$FF,$02,$80,$00
	.byte $02,$A0,$FF,$02,$40,$00,$04,$E0,$FF,$FF,$07,$15,$02,$40,$00,$02
	.byte $A0,$FF,$02,$80,$00,$02,$A0,$FF,$02,$40,$00,$02,$E0,$FF

	NEWTUNE(s_reactor2)	
	.byte $86,t1_reactor2
	.byte $87,t2_reactor2
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$80
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_goosefish,$24)
	.byte $02,$04,$04,$10,$02,$18,$12,$00,$02,$E8,$04,$F0
	
	NEWDATA1(t1_goosefish,$24)
	.byte $02,$10,$06,$0A,$00,$00,$14,$E0,$FF

	NEWTUNE(s_goosefish)	
	.byte $8B,$40
	.byte $86,t1_goosefish
	.byte $87,t2_goosefish
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$C0
	.byte $00,$20
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_blowfish,$25)
	.byte $02,$1C,$10,$04,$28,$00,$02,$FC,$0A,$00,$02,$F8,$08,$00,$02
	.byte $FC,$FF,$08,$07,$06,$00,$02,$FC,$04,$00,$02,$FC,$02,$00,$02,$FC
	
	NEWDATA1(t1_blowfish,$25)
	.byte $02,$60,$00,$02,$A0,$FF,$02,$00,$00,$02,$A0,$00,$02,$00,$00,$02
	.byte $60,$FF,$02,$00,$00,$02,$90,$00,$02,$C0,$FF,$02,$10,$00,$02,$D0
	.byte $FF,$02,$30,$00,$02,$40,$00,$02,$C0,$FF,$02,$A0,$00,$02,$30,$FF
	.byte $02,$00,$00,$02,$30,$01,$02,$00,$00,$02,$60,$FF,$02,$00,$00,$02
	.byte $80,$00,$02,$90,$FF,$02,$50,$00,$02,$F0,$FF,$02,$B0,$FF,$04,$80
	.byte $00,$02,$00,$00,$02,$80,$FF,$02,$D0,$FF,$02,$00,$00,$02,$B0,$00
	.byte $02,$80,$FF,$02,$00,$02,$02,$70,$FE,$02,$00,$01,$02,$00,$FF,$02
	.byte $90,$00,$02,$90,$01,$02,$00,$00,$02,$20,$FF,$02,$00,$00,$02,$B0
	.byte $01,$02,$10,$FF,$02,$20,$FF,$02,$00,$00,$02,$60,$02,$02,$E0,$FE
	.byte $02,$00,$00,$02,$60,$FF,$02,$60,$01,$02,$30,$FE,$02,$F0,$02,$02
	.byte $F0,$FE,$02,$00,$00,$02,$40,$FF,$02,$B0,$02,$02,$10,$FD,$02,$00
	.byte $00,$02,$40,$01,$02,$00,$00,$02,$50,$FF,$02,$00,$00,$02,$D0,$01
	.byte $02,$50,$FF,$02,$C0,$01,$02,$70,$FD,$02,$00,$00,$02,$40,$01,$02
	.byte $00,$00,$02,$80,$FF,$02,$00,$00,$02,$70,$01,$02,$E0,$FE,$02,$B0
	.byte $00,$02,$00,$00,$02,$F0,$00,$02,$A0,$FD,$02,$40,$01,$02,$00,$00
	.byte $02,$70,$FF,$02,$30,$01,$02,$B0,$00,$02,$D0,$FD,$02,$00,$00,$02
	.byte $10,$01,$02,$00,$00,$02,$D0,$01,$02,$00,$00
	
	NEWTUNE(s_blowfish)
	.byte $8B,$80
	.byte $86,t1_blowfish
	.byte $87,t2_blowfish
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$B4
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_blowfish2,$26)
	.byte $02,$1C,$10,$04,$28
	.byte $00,$02,$FC,$0A,$00,$02,$F8,$08,$00,$02,$FC,$FF,$08,$07,$06,$00
	.byte $02,$FC,$04,$00,$02,$FC,$02,$00,$02,$FC
	
	NEWDATA1(t1_blowfish2,$26)
	.byte $02,$60,$00,$02,$A0,$FF
	.byte $02,$00,$00,$02,$A0,$00,$02,$00,$00,$02,$60,$FF,$02,$00,$00,$02
	.byte $90,$00,$02,$C0,$FF,$02,$10,$00,$02,$D0,$FF,$02,$30,$00,$02,$40
	.byte $00,$02,$C0,$FF,$02,$A0,$00,$02,$30,$FF,$02,$00,$00,$02,$30,$01
	.byte $02,$00,$00,$02,$60,$FF,$02,$00,$00,$02,$80,$00,$02,$90,$FF,$02
	.byte $50,$00,$02,$F0,$FF,$02,$B0,$FF,$04,$80,$00,$02,$00,$00,$02,$80
	.byte $FF,$02,$D0,$FF,$02,$00,$00,$02,$B0,$00,$02,$80,$FF,$02,$00,$02
	.byte $02,$70,$FE,$02,$00,$01,$02,$00,$FF,$02,$90,$00,$02,$90,$01,$02
	.byte $00,$00,$02,$20,$FF,$02,$00,$00,$02,$B0,$01,$02,$10,$FF,$02,$20
	.byte $FF,$02,$00,$00,$02,$60,$02,$02,$E0,$FE,$02,$00,$00,$02,$60,$FF
	.byte $02,$60,$01,$02,$30,$FE,$02,$F0,$02,$02,$F0,$FE,$02,$00,$00,$02
	.byte $40,$FF,$02,$B0,$02,$02,$10,$FD,$02,$00,$00,$02,$40,$01,$02,$00
	.byte $00,$02,$50,$FF,$02,$00,$00,$02,$D0,$01,$02,$50,$FF,$02,$C0,$01
	.byte $02,$70,$FD,$02,$00,$00,$02,$40,$01,$02,$00,$00,$02,$80,$FF,$02
	.byte $00,$00,$02,$70,$01,$02,$E0,$FE,$02,$B0,$00,$02,$00,$00,$02,$F0
	.byte $00,$02,$A0,$FD,$02,$40,$01,$02,$00,$00,$02,$70,$FF,$02,$30,$01
	.byte $02,$B0,$00,$02,$D0,$FD,$02,$00,$00,$02,$10,$01,$02,$00,$00,$02
	.byte $D0,$01,$02,$00,$00

	NEWTUNE(s_blowfish2)	
	.byte $8B,$80
	.byte $86,t1_blowfish2
	.byte $87,t2_blowfish2
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$80
	.byte $00,$B4
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_circfish,$27)
	.byte $14,$04,$12,$00
	.byte $02,$F0,$02,$FC
	.byte $02,$00,$02,$FC
	.byte $16,$00,$02,$FC
	.byte $02,$00,$02,$FC
	.byte $02,$00,$02,$FC
	.byte $02,$00,$02,$FC
	.byte $2C,$00
	
	NEWDATA1(t1_circfish,$27)
	.byte $02,$F0
	.byte $0F,$24,$00,$00
	.byte $02,$F0,$FE,$56
	.byte $00,$00
	
	NEWTUNE(s_circfish)
	.byte $86,t1_circfish
	.byte $87,t2_circfish
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$7E
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_circfish2,$28)
	.byte $0A,$04,$06,$00,$02,$04,$08,$00,$06,$FC,$08,$00,$02,$04
	.byte $1A,$00,$04,$FC,$06,$00,$04,$FC,$2C,$00
	
	NEWDATA1(t1_circfish2,$28)
	.byte $02,$E0,$0F,$24,$00,$00,$02,$10,$FF,$56,$00,$00
	
	NEWTUNE(s_circfish2)
	.byte $86,t1_circfish2
	.byte $87,t2_circfish2
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$7E
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_feject,$29)
	.byte $02,$3C,$06,$F8,$02,$FC,$02,$00,$02,$FC,$02,$00,$02,$FC,$04,$00,$02
	.byte $FC,$12,$00,$02,$FC,$06,$00,$02,$FC,$0A,$00,$02,$FC,$0C,$00,$02
	.byte $FC,$0A,$00
	
	NEWDATA1(t1_feject,$29)
	.byte $02,$60,$02,$04,$40,$FF,$02,$30,$FF,$38,$10,$00,$02
	.byte $00,$00,$02,$20,$00,$14,$10,$00

	NEWTUNE(s_feject)		
	.byte $86,t1_feject
	.byte $87,t2_feject
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$60
	.byte $00,$58
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_fshot,$2a)
	.byte $02,$30,$04,$00,$02,$04,$02,$00,$02,$04
	.byte $02,$00,$02,$04,$04,$FC,$02,$00,$02,$FC,$04,$00,$02,$FC,$06,$00
	.byte $02,$FC,$06,$00,$02,$FC,$04,$00,$02,$FC,$06,$00,$02,$FC,$FF,$03
	.byte $07,$0C,$00,$02,$FC,$0E,$00,$02,$FC,$0E,$00,$02,$FC,$0E,$00,$02
	.byte $FC
	
	NEWDATA1(t1_fshot,$2a)
	.byte $02,$20,$07,$0C,$E0,$FF,$02,$D0,$F9,$7E,$40,$00,$02,$10,$F0
	.byte $02,$00,$00

	NEWTUNE(s_fshot)	
	.byte $86,t1_fshot
	.byte $87,t2_fshot
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$C0
	.byte $00,$0C
	.byte $8A,$A0
	.byte $00,$03
	.byte $8A,$80
	.byte $00,$27
	.byte $8A,$C0
	.byte $00,$4D
	.byte $8A,$A0
	.byte $00,$13
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_mzassem,$2b)
	NEWDATA1(t1_mzassem,$2b)
	NEWTUNE(s_mzassem)	
	.byte $00,$00

;**************************************
	NEWDATA2(t2_mzassem2,$2c)
	NEWDATA1(t1_mzassem2,$2c)
	NEWTUNE(s_mzassem2)	
	.byte $00,$00

;**************************************
	NEWDATA2(t2_mzassem3,$2d)
	NEWDATA1(t1_mzassem3,$2d)
	NEWTUNE(s_mzassem3)	
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_blowspin,$2e)
	.byte $04,$00,$02,$10,$02,$14,$02,$18,$3E
	.byte $00,$1C,$FC
	
	NEWDATA1(t1_blowspin,$2e)
	.byte $02,$30,$06,$2E,$F0,$FF,$32,$10,$00
	
	NEWTUNE(s_blowspin)	
	.byte $8B,$40
	.byte $86,t1_blowspin
	.byte $87,t2_blowspin
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$40
	.byte $00,$62
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_mazehit,$2f)
	.byte $12,$04,$06,$08
	
	NEWDATA1(t1_mazehit,$2f)
	.byte $02,$30,$00,$16,$00,$00
	
	NEWTUNE(s_mazehit)	
	.byte	$86,t1_mazehit
	.byte $87,t2_mazehit
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$40
	.byte $00,$18
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_exp7,$30)
	.byte $02,$3C,$0E,$00,$02,$F4,$06,$F8,$02,$F4,$02,$F8
	.byte $02,$FC
	
	NEWDATA1(t1_exp7,$30)
	.byte $02,$A0,$08,$02,$00,$00,$02,$20,$02,$02,$00,$00,$02,$50
	.byte $02,$02,$00,$00,$02,$B0,$F4,$10,$00,$00
	
	NEWTUNE(s_exp7)	
	.byte $86,t1_exp7
	.byte $87,t2_exp7
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$C0
	.byte $00,$0C
	.byte $8A,$00
	.byte $00,$13
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_lshot,$31)
	.byte $02,$3C,$02,$00
	.byte $02,$F8,$02,$00,$02,$F8,$02,$00,$02,$F8,$02,$00,$02,$F0,$02,$00
	.byte $02,$04,$12,$00,$02,$04,$06,$00,$08,$04,$04,$08
	
	NEWDATA1(t1_lshot,$31)
	.byte $02,$60,$02,$02
	.byte $00,$00,$02,$40,$FF,$02,$00,$00,$02,$40,$FF,$02,$00,$00,$02,$30
	.byte $FF,$1C,$00,$00,$02,$10,$00,$08,$00,$00,$02,$10,$00,$06,$00,$00
		
	NEWTUNE(s_lshot)	
	.byte $86,t1_lshot
	.byte $87,t2_lshot
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$10
	.byte $8A,$00
	.byte $00,$2D
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_hitshield,$32)
	.byte $02,$04,$02,$00,$02,$04,$02,$08,$06,$0C,$02,$CC,$18,$00
	
	NEWDATA1(t1_hitshield,$32)
	.byte $02,$80,$00,$1C,$00,$00,$02,$80,$FF,$08,$00,$00
	
	NEWTUNE(s_hitshield)	
	.byte $86,t1_hitshield
	.byte $87,t2_hitshield
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$40
	.byte $00,$12
	.byte $8A,$00
	.byte $00,$17
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_noshield,$33)
	.byte $02,$18,$04,$00,$02,$04,$04,$00,$02,$04,$02,$00,$04,$04,$02,$08,$02,$0C
	
	NEWDATA1(t1_noshield,$33)
	.byte $02,$20,$04,$02,$00,$00,$02,$F0,$FF,$02,$00,$00,$06,$F0,$FF,$04
	.byte $E0,$FF,$02,$C0,$FF,$02,$A0,$FF,$02,$80,$FF
	
	NEWTUNE(s_noshield)	
	.byte $86,t1_noshield
	.byte $87,t2_noshield
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$18
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_manhit,$34)
	.byte $02,$14,$08,$00,$02,$04,$04
	.byte $00,$02,$04,$02,$00,$FF,$02,$07,$08,$04,$02,$08,$0A,$00,$02,$C4
	
	NEWDATA1(t1_manhit,$34)
	.byte $02,$30,$00,$30,$00,$00
	
	NEWTUNE(s_manhit)	
	.byte $86,t1_manhit
	.byte $87,t2_manhit
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$40
	.byte $00,$32
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_mantrip,$35)
	.byte $02,$20,$04,$00,$02,$F8,$02,$FC,$06,$0C,$02,$04
	.byte $06,$00,$02,$FC,$02,$00,$FF,$03,$07,$0E,$F8,$02,$FC,$10,$00
	
	NEWDATA1(t1_mantrip,$35)
	.byte $02,$50,$01,$04,$E0,$FF,$02,$F0,$FF,$02,$30,$03,$02,$20,$00,$02,$10
	.byte $00,$02,$00,$00,$02,$E0,$FF,$04,$D0,$FF,$02,$E0,$FF,$02,$F0,$FF
	.byte $02,$E0,$FF,$02,$00,$00,$02,$D0,$FF,$02,$F0,$FF,$02,$E0,$FF,$02
	.byte $00,$00,$04,$F0,$FF,$04,$00,$00,$02,$F0,$FF,$04,$00,$00,$02,$F0
	.byte $FF,$08,$00,$00,$02,$10,$00,$06,$00,$00
	
	NEWTUNE(s_mantrip)	
	.byte $8B,$01
	.byte $86,t1_mantrip
	.byte $87,t2_mantrip
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$E0
	.byte $00,$46
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_manswon,$36)
	NEWDATA1(t1_manswon,$36)
	NEWTUNE(s_manswon)	
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_manswoff,$37)
	NEWDATA1(t1_manswoff,$37)
	NEWTUNE(s_manswoff)	
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_spikes,$38)
	.byte $02,$04,$02,$00,$02,$04,$02,$08,$28,$00,$02,$FC,$30,$00,$02,$FC,$2E,$00
	.byte $02,$FC,$26,$00
	
	NEWDATA1(t1_spikes,$38)
	.byte $02,$F0,$0F,$B8,$00,$00
	
	NEWTUNE(s_spikes)	
	.byte $8B,$C1
	.byte $86,t1_spikes
	.byte $87,t2_spikes
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$80
	.byte $00,$BA
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_spikes2,$39)
	.byte $02,$04,$02,$00,$02,$04
	.byte $08,$08,$02,$14,$12,$00,$02,$FC,$08,$00,$02,$FC,$06,$00,$02,$FC
	.byte $06,$00,$02,$FC,$06,$00,$02,$FC,$08,$00,$02,$FC,$06,$00,$02,$FC
	.byte $08,$00,$02,$FC,$06,$00,$02,$FC,$08,$00,$02,$FC,$08,$00,$02,$FC
	.byte $0E,$00,$02,$FC,$12,$00,$02,$FC,$14,$00,$02,$FC,$02,$00
	
	NEWDATA1(t1_spikes2,$39)
	.byte $02,$D0,$0F,$B8,$00,$00
	
	NEWTUNE(s_spikes2)	
	.byte $8B,$C1
	.byte $86,t1_spikes2
	.byte $87,t2_spikes2
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$BA
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_spikes3,$3a)
	.byte $02,$3C,$04,$00,$02,$F4,$06,$F8,$02,$F4,$02,$F8
	
	NEWDATA1(t1_spikes3,$3a)
	.byte $02,$70,$00,$10,$00,$00
	
	NEWTUNE(s_spikes3)	
	.byte $8B,$C1
	.byte $86,t1_spikes3
	.byte $87,t2_spikes3
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$00
	.byte $00,$12
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_key,$3b)
	.byte $02,$30,$02,$FC,$02,$F4,$02,$18,$02,$F0
	.byte $02,$0C,$02,$00,$04,$F8,$02,$08,$02,$F8,$02,$F4,$02,$F8,$02,$0C
	.byte $02,$FC,$02,$08,$02,$04,$02,$08,$02,$F4,$02,$14,$02,$08,$02,$F8
	.byte $02,$04,$02,$EC,$02,$FC,$02,$08,$02,$F8,$02,$04,$02,$F4,$02,$0C
	.byte $02,$F0,$02,$08,$02,$04,$02,$EC,$02,$10,$02,$F0,$02,$0C,$02,$F0
	.byte $02,$08,$02,$FC,$02,$08,$02,$F4,$04,$04,$04,$FC,$02,$08,$08,$FC
	.byte $08,$00
	
	NEWDATA1(t1_key,$3b)
	.byte $02,$50,$03,$6C,$00,$00
	
	NEWTUNE(s_key)	
	.byte $8B,$40
	.byte $86,t1_key
	.byte $87,t2_key
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$C0
	.byte $00,$6E
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_door,$3c)
	.byte $02,$3C,$04,$00,$02,$FC,$04,$00
	.byte $02,$FC,$02,$00,$04,$FC,$06,$00,$04,$FC,$04,$00,$06,$FC,$08,$00
	.byte $02,$FC,$FF,$05,$07
	
	NEWDATA1(t1_door,$3c)
	.byte $02,$70,$00,$62,$00,$00
	
	NEWTUNE(s_door)	
	.byte $8B,$01
	.byte $86,t1_door
	.byte $87,t2_door
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$80
	.byte $00,$06
	.byte $8A,$20
	.byte $00,$07
	.byte $8A,$80
	.byte $00,$57
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_rhum,$3d)
	NEWDATA1(t1_rhum,$3d)
	NEWTUNE(s_rhum)	
	.byte $00,$00

;**************************************
	NEWDATA2(t2_reactor3,$3e)
	NEWDATA1(t1_reactor3,$3e)
	NEWTUNE(s_reactor3)	
	.byte $00,$00

;**************************************
	NEWDATA2(t2_handact,$3f)
	NEWDATA1(t1_handact,$3f)
	NEWTUNE(s_handact)	
	.byte $00,$00

;**************************************
	NEWDATA2(t2_handact2,$40)
	NEWDATA1(t1_handact2,$40)
	NEWTUNE(s_handact2)	
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_handoff,$41)
	.byte $02,$3C,$04,$00,$08
	.byte $00,$02,$6C,$FF,$0A,$07,$02,$EC,$02,$10,$0C,$00,$02,$F0,$02,$10
	.byte $06,$00,$02,$F0,$02,$0C,$08,$00,$02,$F4,$02,$0C,$06,$00,$02,$FC
	.byte $04,$00,$02,$F8,$02,$08,$04,$00,$02,$F8,$02,$04,$02,$FC,$FF,$04
	.byte $07
	
	NEWDATA1(t1_handoff,$41)
	.byte $02,$00,$0C,$02,$00,$00,$02,$10,$00,$FF,$1B,$09,$02,$00,$00
	.byte $02,$10,$00,$FF,$06,$09,$02,$00,$00,$02,$20,$00,$FF,$0B,$09,$02
	.byte $00,$00,$02,$30,$F0
	
	NEWTUNE(s_handoff)	
	.byte $8B,$01
	.byte $86,t1_handoff
	.byte $87,t2_handoff
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$C8
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_handon,$42)
	.byte $02,$04,$02,$FC,$FF,$04,$07,$02,$08,$04,$00
	.byte $02,$F8,$02,$08,$04,$00,$02,$F8,$02,$08,$02,$00,$02,$04,$08,$00
	.byte $02,$F4,$02,$0C,$06,$00,$02,$04,$06,$00,$02,$F0,$02,$10,$08,$00
	.byte $02,$04,$09,$00,$FF,$10,$07
	
	NEWDATA1(t1_handon,$42)
	.byte $02,$D0,$0F,$02,$00,$00,$02,$E0,$FF
	.byte $FF,$0B,$09,$02,$00,$00,$02,$F0,$FF,$FF,$24,$09,$04,$00,$00
	
	NEWTUNE(s_handon)	
	.byte $8B,$01
	.byte $86,t1_handon
	.byte $87,t2_handon
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$CA
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_tickfast,$43)
	NEWDATA1(t1_tickfast,$43)
	NEWTUNE(s_tickfast)	
	.byte $00,$00

;**************************************
	NEWDATA2(t2_tickslow,$44)
	NEWDATA1(t1_tickslow,$44)
	NEWTUNE(s_tickslow)	
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_trans,$45)
	.byte $02,$04,$04,$00,$02,$04,$02,$00,$02,$04,$02,$00,$04
	.byte $04,$04,$08,$04,$0C,$48,$00,$1C,$FC
	
	NEWDATA1(t1_trans,$45)
	.byte $02,$30,$00,$7C,$00,$00
	
	NEWTUNE(s_trans)	
	.byte $8B,$64
	.byte $86,t1_trans
	.byte $87,t2_trans
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$40
	.byte $00,$7E
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_trans2,$46)
	.byte $04,$00
	
	NEWDATA1(t1_trans2,$46)
	.byte $02,$A0,$0F
	
	NEWTUNE(s_trans2)	
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_cann,$47)
	.byte $10,$00,$02,$38,$02,$C8,$2A,$00
	
	NEWDATA1(t1_cann,$47)
	.byte $3E,$00,$00

	NEWTUNE(s_cann)	
	.byte $86,t1_cann
	.byte $87,t1_cann
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$40
	.byte $00,$3C
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_cann2,$48)
	.byte $1E,$04,$02,$00,$1C,$FC
	
	NEWDATA1(t1_cann2,$48)
	.byte $02,$A0,$00,$38,$00,$00,$02,$60,$FF
	
	NEWTUNE(s_cann2)	
	.byte $86,t1_cann2
	.byte $87,t2_cann2
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$40
	.byte $00,$3C
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_cann3,$49)
	.byte $10,$00,$02,$3C
	.byte $02,$00,$02,$C4,$02,$00,$02,$3C,$06,$00,$0E,$F8,$02,$FC,$0E,$00
	
	NEWDATA1(t1_cann3,$49)
	.byte $02,$30,$00,$3A,$00,$00
	
	NEWTUNE(s_cann3)	
	.byte $86,t1_cann3
	.byte $87,t2_cann3
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$00
	.byte $00,$3C
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_cann4,$4a)
	.byte $10,$00,$02,$3C,$1E,$FC,$0E,$00
	
	NEWDATA1(t1_cann4,$4a)
	.byte $12,$00,$00,$28
	.byte $10,$00,$04,$00,$00
	
	NEWTUNE(s_cann4)	
	.byte $86,t1_cann4
	.byte $87,t2_cann4
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$80
	.byte $00,$3C
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_ssplash,$4b)
	.byte $02,$3C,$06,$00,$1E,$FC,$0C,$00
	
	NEWDATA1(t1_ssplash,$4b)
	.byte $02,$30,$00,$2A,$00
	.byte $00,$02,$70,$00,$04,$00,$00
	
	NEWTUNE(s_ssplash)	
	.byte $86,t1_ssplash
	.byte $87,t2_ssplash
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$00
	.byte $00,$2C
	.byte $8A,$C0
	.byte $00,$05
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_rblow,$4c)
	.byte $02,$3C,$06,$00,$02,$C4,$06
	.byte $00,$02,$3C,$04,$FC,$02,$00,$02,$FC,$04,$00,$02,$FC,$06,$00,$02
	.byte $FC,$08,$00,$02,$FC,$0A,$00,$02,$FC,$0E,$00,$02,$FC,$10,$00,$02
	.byte $FC,$12,$00,$02,$FC,$14,$00,$02,$FC,$16,$00,$02,$FC,$18,$00,$02
	.byte $FC,$1A,$00,$02,$FC,$10,$00,$02,$FC
	
	NEWDATA1(t1_rblow,$4c)
	.byte $02,$C0,$03,$E8,$00,$00
	
	NEWTUNE(s_rblow)	
	.byte $86,t1_rblow
	.byte $87,t2_rblow
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$40
	.byte $00,$EA
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_rblow2,$4d)
	.byte $02,$3C,$06
	.byte $00,$02,$C4,$06,$00,$02,$3C,$04,$FC,$02,$00,$02,$FC,$04,$00,$02
	.byte $FC,$06,$00,$02,$FC,$08,$00,$02,$FC,$0A,$00,$02,$FC,$0E,$00,$02
	.byte $FC,$10,$00,$02,$FC,$12,$00,$02,$FC,$14,$00,$02,$FC,$16,$00,$02
	.byte $FC,$18,$00,$02,$FC,$1A,$00,$02,$FC,$16,$00,$02,$FC
	
	NEWDATA1(t1_rblow2,$4d)
	.byte $02,$80,$01
	.byte $04,$F0,$FF,$08,$10,$00,$08,$F0,$FF,$FF,$0D,$09,$08,$10,$00,$02
	.byte $60,$FE
	
	NEWTUNE(s_rblow2)	
	.byte $86,t1_rblow2
	.byte $87,t2_rblow2
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$00
	.byte $00,$F0
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_rblow3,$4e)
	.byte $02,$3C,$02,$00,$02,$C4,$08,$00,$02,$3C,$06,$FC,$02,$00,$02,$FC
	.byte $FF,$02,$07,$04,$00,$02,$FC,$FF,$02,$07,$06,$00,$02,$FC,$FF,$02
	.byte $07,$08,$00,$02,$FC,$08,$00,$02,$FC,$0A,$00,$02,$FC
	
	NEWDATA1(t1_rblow3,$4e)
	.byte $02,$30,$02,$0A,$F0,$FF,$02,$D0,$FE,$5E,$10,$00
	
	NEWTUNE(s_rblow3)	
	.byte $86,t1_rblow3
	.byte $87,t2_rblow3
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$20
	.byte $00,$32
	.byte $8A,$00
	.byte $00,$03
	.byte $8A,$20
	.byte $00,$39
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_secthrust,$4f)
	NEWDATA1(t1_escthrust,$4f)
	NEWTUNE(s_escthrust)	
	.byte $00,$00

;**************************************
	NEWDATA2(t2_escthrust2,$50)
	NEWDATA1(t1_escthrust2,$50)
	NEWTUNE(s_escthrust2)	
	.byte $00,$00

;**************************************
	NEWDATA2(t2_escdie,$51)
	NEWDATA1(t1_escdie,$51)
	NEWTUNE(s_escdie)	
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_escfall,$52)
	.byte $02,$08,$FE,$00,$FE,$00,$FE,$00,$FE,$00
	
	NEWDATA1(t1_escfall,$52)
	.byte $3E,$00,$00,$02,$10,$00,$FF,$0E,$09,$3A,$00,$00
	
	NEWTUNE(s_escfall)	
	.byte $86,t1_escfall
	.byte $87,t2_escfall
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$00
	.byte $00,$02
	.byte $8A,$00
	.byte $00,$FF
	.byte $8A,$00
	.byte $00,$FF
	.byte $8A,$00
	.byte $00,$FF
	.byte $8A,$00
	.byte $00,$FF
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_esccrash,$53)
	NEWDATA1(t1_esccrash,$53)
	NEWTUNE(s_esccrash)	
	.byte $00,$00

;**************************************
	NEWDATA2(t2_lineback,$54)
	NEWDATA1(t1_lineback,$54)
	NEWTUNE(s_lineback)	
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_mazekill,$55)
	.byte $04,$00,$02
	.byte $04,$02,$00,$02,$04,$04,$00,$02,$04,$06,$00,$02,$04,$08,$00,$FF
	.byte $08,$07,$02,$08,$0A,$00,$02,$04,$28,$00,$10,$FC
	
	NEWDATA1(t1_mazekill,$55)
	.byte $02,$50,$08,$02
	.byte $00,$00,$02,$30,$FE,$02,$00,$00,$02,$F0,$FE,$02,$00,$00,$02,$30
	.byte $02,$02,$50,$FF,$02,$D0,$FE,$02,$90,$00,$02,$00,$00,$02,$C0,$FE
	.byte $02,$60,$02,$02,$10,$FF,$02,$00,$00,$02,$50,$FF,$02,$20,$01,$02
	.byte $90,$FE,$02,$00,$00,$02,$80,$00,$02,$00,$00,$02,$C0,$FE,$02,$00
	.byte $00,$02,$90,$02,$02,$40,$FE,$02,$B0,$00,$02,$30,$FE,$02,$00,$00
	.byte $02,$B0,$00,$02,$00,$00,$02,$C0,$FE,$02,$00,$00,$02,$F0,$02,$02
	.byte $50,$FD,$02,$C0,$00,$02,$00,$00,$02,$10,$01,$02,$10,$FD,$02,$D0
	.byte $01,$02,$A0,$FE,$02,$A0,$00,$02,$00,$00,$02,$20,$01,$02,$A0,$FD
	.byte $02,$00,$00,$02,$E0,$00,$02,$F0,$00,$02,$50,$FE,$02,$00,$00,$02
	.byte $E0,$00,$02,$00,$00,$02,$70,$FE,$02,$70,$FF,$02,$00,$01,$02,$00
	.byte $FF,$02,$90,$01,$02,$00,$FE,$02,$80,$00,$02,$50,$FF,$02,$00,$00
	.byte $02,$30,$00,$02,$80,$00,$02,$00,$00,$04,$80,$FF,$02,$50,$00,$02
	.byte $10,$00,$02,$B0,$FF,$02,$70,$00,$02,$80,$FF,$02,$00,$00,$02,$A0
	.byte $00,$02,$00,$00,$02,$D0,$FE,$02,$00,$00,$02,$D0,$00,$02,$60,$FF
	.byte $02,$40,$00,$02,$C0,$FF,$02,$D0,$FF,$02,$30,$00,$02,$F0,$FF,$02
	.byte $40,$00,$02,$70,$FF,$02,$00,$00,$02,$A0,$00,$02,$00,$00,$02,$60
	.byte $FF,$02,$00,$00,$02,$60,$00
	
	NEWTUNE(s_mazekill)	
	.byte $8B,$80
	.byte $86,t1_mazekill
	.byte $87,t2_mazekill
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$40
	.byte $00,$02
	.byte $8A,$A0
	.byte $00,$B3
	.byte $00,$00
	
;**************************************
	NEWDATA2(t2_mazekill2,$56)
	.byte $04,$00,$02,$04,$02
	.byte $00,$02,$04,$04,$00,$02,$04,$06,$00,$02,$04,$08,$00,$FF,$08,$07
	.byte $02,$08,$0A,$00,$02,$04,$28,$00,$10,$FC
	
	NEWDATA1(t1_mazekill2,$56)
	.byte $02,$50,$08,$02,$00,$00
	.byte $02,$30,$FE,$02,$00,$00,$02,$F0,$FE,$02,$00,$00,$02,$30,$02,$02
	.byte $50,$FF,$02,$D0,$FE,$02,$90,$00,$02,$00,$00,$02,$C0,$FE,$02,$60
	.byte $02,$02,$10,$FF,$02,$00,$00,$02,$50,$FF,$02,$20,$01,$02,$90,$FE
	.byte $02,$00,$00,$02,$80,$00,$02,$00,$00,$02,$C0,$FE,$02,$00,$00,$02
	.byte $90,$02,$02,$40,$FE,$02,$B0,$00,$02,$30,$FE,$02,$00,$00,$02,$B0
	.byte $00,$02,$00,$00,$02,$C0,$FE,$02,$00,$00,$02,$F0,$02,$02,$50,$FD
	.byte $02,$C0,$00,$02,$00,$00,$02,$10,$01,$02,$10,$FD,$02,$D0,$01,$02
	.byte $A0,$FE,$02,$A0,$00,$02,$00,$00,$02,$20,$01,$02,$A0,$FD,$02,$00
	.byte $00,$02,$E0,$00,$02,$F0,$00,$02,$50,$FE,$02,$00,$00,$02,$E0,$00
	.byte $02,$00,$00,$02,$70,$FE,$02,$70,$FF,$02,$00,$01,$02,$00,$FF,$02
	.byte $90,$01,$02,$00,$FE,$02,$80,$00,$02,$50,$FF,$02,$00,$00,$02,$30
	.byte $00,$02,$80,$00,$02,$00,$00,$04,$80,$FF,$02,$50,$00,$02,$10,$00
	.byte $02,$B0,$FF,$02,$70,$00,$02,$80,$FF,$02,$00,$00,$02,$A0,$00,$02
	.byte $00,$00,$02,$D0,$FE,$02,$00,$00,$02,$D0,$00,$02,$60,$FF,$02,$40
	.byte $00,$02,$C0,$FF,$02,$D0,$FF,$02,$30,$00,$02,$F0,$FF,$02,$40,$00
	.byte $02,$70,$FF,$02,$00,$00,$02,$A0,$00,$02,$00,$00,$02,$60,$FF,$02
	.byte $00,$00,$02,$60,$00
	
	NEWTUNE(s_mazekill2)	
	.byte $8B,$80
	.byte $86,t1_mazekill2
	.byte $87,t2_mazekill2
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$80
	.byte $00,$B4
	.byte $00,$00

;**************************************
	NEWDATA2(t2_trans3,$57)
	NEWDATA1(t1_trans3,$57)
	NEWTUNE(s_trans3)	
	.byte	$00,$00

;**************************************
	NEWDATA2(t2_trans4,$58)
	NEWDATA1(t1_trans4,$58)
	NEWTUNE(s_trans4)	
	.byte $00,$00

;**************************************
; Good Place for a csum
	.byte $CD

;*****************************************************
		.sbttl "Coin Counter Update"
;*****************************************************
framecntl	lda	#00			;Guess Off
		ldy	cnt1			;Mech On??
		ifmi
			lda	#counter2		;Turn on this one
		endif
		ldy	cnt2
		ifmi
			ora	#counter1
		endif
		sta	counter			;Output Pulse
		rts	
		
;*****************************************************
		.sbttl "Game IRQ"
;*****************************************************
;* This routine is called from the main IRQ for RPM, *
;* it takes care of game specific tasks outside the  *
;* functionality of RPM itself.                      *
;*                                                   *
;* Inputs: A = Frame Counter                         *
;*****************************************************	
game_irq	pha				;Save counter for later too	
		and	#03
		ifeq
			jsr	eeupd			;Update EEROM every 4 frames
		endif
		pla				;Get Original counter
		lsr	A
		ifcc				;Every Other Time, do the coin counters
			ldy	#00			;Say counters off
			ldx	#01			;Do both counters
			begin
				lda	cnt1,X		;Counter On??
				ifne
					cmp	#$10
					ifcs
						adc	#$EF		;Sub 4 (carry set)
						iny			;Got one on
						sta	cnt1,X
					endif
				endif
				dex
			miend
			tya			;Any on?
			bne	?si10		;Yep
			;None on, see if we can start any
			ldx	#01
			begin
				lda	cnt1,X
				ifne
					clc	
					adc	#$EF			;Set 4 MSB, dec 4 LSB
					sta	cnt1,X		
					bmi	?si10			;Exit so only 1 start
				endif				
				dex	
			miend
		endif
?si10		;See if 'Reset Score' request came in
		lda	newreq		;Request??
		ifmi				;yep
			dec	newreq		;Wait time up??
			ifmi				;yep
				ldx	teax+2		;Index of scores
				lda	eerom+2,X		;Clobber a score
				eor	#$30
				sta	eerom+2,X		;Put it back
				lda	#00
				sta	newreq		;And clear request
			endif
		endif
		rts

;*********************************************
		.sbttl "Utility Routines"
;*********************************************
neg		eor	#$FF
		clc
		adc	#01
		rts
		
		
;*********************************************
;* Exceptions which do not return data
;*********************************************
getsta	;Not Implemented
sendst	;Not Implemented

coin1		inc	cnt1
		jmp	exit2
coin2		inc	cnt2
		jmp	exit2
		
;********************************************
;* Used to clobber checksums on gamma so next
;* restart will cause gamma to re-init scores
newscr	lda	#00
		sta	eezflg		;Clear all EEROM flags
		sta	eerequ
		sta	eerwrq
		lda	#$80
		sta	newreq		;Request interrupt clobber scores
		ldx	teax			;Get index of scores
		lda	eerom+2,X		;Clobber a score
		eor	#$30			;Just clobber it
		sta	eerom+2,X		;Clobber a score
		lda	#10d
		sta	reqwai		;Wait 10 interrupts till next clobber
		jmp	exit2
		
newst		jsr	eezboo		;Clear Bookkeeping
		jmp	exit2
		
option0	sta	potgo			;Read switches
		nop	
		lda	allpot		;Get input
		jmp	exit
		
option1	lda	intack		;Get option switches
		jmp	exit			;Send them home
		
pokran	lda	pokey1+random
		jmp	exit			;Leave with a random #
		
switch	lda	input			;Where the switches are
		jmp	exit
		
rolgig	lda	leta			
exit		sta	outdata
exit2		pla				;Exit for no data back
		tax	
		pla	
		tay	
		lda	#-1
		sta	datnum
		pla	
		rti	
	
;*******************************************
		.sbttl "Default Scores and Initials"
;*******************************************
dscore	.byte $32,$45,$09,$00,$17,$82,$08,$00,$85,$72,$07,$00

dinit		.byte $0B,$1F,$15,$0B,$1F,$15,$0B,$1F,$15

;*******************************************
		.sbttl "Poweron Self Test"
;*******************************************
r_sysst	lda	#00
		sta	outdata		;Ack start but not ready
		
;*******************************************
;* Entry from MainLine Start
selftest	lda	#00
		sta	rambad
		sta	rambad+1
		sta	rambad+2
		sta	rambad+3		;Clear any 'old' errors
		lda	#$FF
		ldx	#00
		begin
			sta	$0007,X		;Clear 0 page
			dex
		eqend				;Clear a section
?st20		lda	$0007,X
		eor	#$FF
		bne	?st33
?st30		sta	$0007,X
		tay	
		eor	$0007,X
		bne	?st33			;Not the same, oh shit, an error
		tya	
		beq	?st35			;Stop when RAM is 0
		eor	#$FF
		sta	$0007,X
		eor	$0007,X
		bne	?st33
		tya	
		asl	A
		jmp	?st30
?st33		ldy	#01
		sty	rambad		;Indicate RAM is bad
?st35		inx	
		bne	?st20			;Continue
;At this point we know 7 page is okay
		stx	tstart		;Do the rest now
		lda	#06
		sta	tend
		stx	tstart+1
		dex	
		stx	tend+1		;Test 0-6ff
		jsr	tst2k			;Test 2K
zpgtst	ifcs
			ldy	#01
			sty	rambad		;Have a RAM error
		endif
		
;********************************************
;* RAM Done... Now checksum ROM and make sure
;* it's okay(this is dumb as how would we be 
;* running if it was nfg!!)
;********************************************
		.sbttl "ROM Test"
;********************************************	
		lda	#00
		tay	
		tax	
		sta	temp1
		lda	#($8000/$100)
		sta	temp1+1
		lda	#64d
		sta	temp2
		txa				;Seed for checksum
		begin
			begin
				eor	(temp1,Y)
				iny
			eqend
			inc	temp1+1
			dec	temp2
		eqend
		sta	rambad+1		;Save Checksum
;********************************************
		.sbttl "Pokey Test"
;********************************************
		ldy	#$1A			;Start with top pokey
		begin
			ldx	#05
			lda	pokey1+audctl,Y
			begin
				cmp	pokey1+audctl,Y
				bne	ok1			;Make sure random
				dex
			miend
			lda	#01
			sta	rambad+2		;Bad Pokey
ok1			tya	
			sec	
			sbc	#08			;Next Pokey
			tay
		miend
;********************************************
		.sbttl "EEROM startup procedure"
;********************************************
		lda	#00
		sta	rambad+3		;Guess OK
		sta	eerdy
		jsr	rehiin		;Read in everything (this goes fast)
		lda	#03
		bit	eebad			;Bad copy??
		ifne				;yep, copy over default
			ldx	#9-1			;Replace with default initials
			begin
				lda	dinit,X		;Get default for intials too
				sta	initl,X
				dex
			miend
			ldx	#12d-1		;Xfer scores
			begin
				lda	dscore,X
				sta	hscore,X
				dex
			miend				;If bad, move in default
			inc	rambad+3		;Another try here
			jsr	wrini			;If yes, try and rewrite default
			jsr	wrhs
		endif
		lda	#$80
		sta	eerdy			;EE stuff is ready
		lda	#00
		ldx	#03
		begin
			sec
			ldy	rambad,X
			ifne
				clc
			endif
			rol	A		;Set bit
			dex
		miend
		ora	#$F0			;Set Don't care bits
;Return A = Status
		jmp	cindy
		
;********************************************
		.sbttl "Ram Test Section"
;********************************************

soft 		= 1
ramwid	= 8
speed 	= 2
	
tstram
tst2k
ramtest	ldy	#vtend-voltbl
?rt10		lda	voltbl,Y		;Move volitile code into VRAM
		sta	vram,Y
		dey	
		bpl	?rt10
		lda	tstart
		sta	vadh0
		sta	vadh1
		sta	vadh2
		sta	vadh3
		jmp	vram
		
voltbl	ldy	#00
		lda	patend
?vt10		sta	$0100,Y		;Preset with first test pattern
		iny	
		bne	?vt10
		ldx	vadh0
		inc	vadh0
		cpx	tend
		bne	voltbl		;Restore regs and repeat for all pages
?vt20		ldx	#patend-pats
		lda	$0100			;Check the location for the first value	
		eor	pats,X
		bne	?vt50			;RAM failure
?vt30		lda	pats-1,X		;Now check this location with all test patterns
		sta	$0100
		eor	$0100
		bne	?vt50			;RAM failed to hold the new pattern
		dex	
		bne	?vt30			;Try next pattern
		inc	vadl1
		inc	vadl2
		inc	vadl3
		bne	?vt20
		lda	vadh1
		inc	vadh1
		inc	vadh2
		inc	vadh3
		eor	tend
		bne	?vt20			;Try next pattern
		jmp	endtst		;and with zero flag and A=0
?vt50		sec	
		jmp	ramerr
vtend

pats		.byte $00,$FE,$01,$FD,$02,$FB,$04,$F7
		.byte $08,$EF,$10,$DF,$20,$BF,$40,$7F,$80
patend	.byte $FF

endtst	clc				;Return with carry clear
ramerr	jmp	zpgtst		;We know stack is no good now

;*********************************************************
		.sbttl "EEROM Routines"
;*********************************************************
;* The following routines are exception modes which are  *
;* used to get data in and out of the EEROM & back to    *
;* Alpha. They run under NMI and assume that the calling *
;* routine in the Alpha system is smart enough not to    *
;* send another command while any of these are running.  *
;*********************************************************
;* Read in High Scores from Alpha, place in proper RAM   *
;* and start EEROM write of the data                     *
;*********************************************************
gets		ldx	#(hscore/256d)		;MSB of Buffer
		ldy	#(hscore&$ff)		;LSB of Buffer
		lda	#numhs			;Number of High Scores
		jsr	xfer				;Get this data
		txa					;Good Xfer??
		ifeq					;yep
			jsr	wrhs				;Start Write of High Scores
			lda	#g_done
			sta	outdata			;Signal all done
		endif
		jmp	exit2				;And leave the exception
	
;*********************************************************
;* Get Initials and Write to EEROM                       *
;*********************************************************
getin		ldx	#(initl/256d)
		ldy	#(initl&$ff)
		lda	#numit			;Number of Initials
		jsr	xfer
		txa	
		ifeq
			jsr	wrini			;Start Write of initials
			lda	#g_done
			sta	outdata
		endif
		jmp	exit2
		
;*********************************************************
;* Send the High Scores back to Alpha                    *
;*********************************************************
sendhs	ldx	#(hscore/256d)
		ldy	#(hscore&$ff)
		lda	#numhs			;Address and count
		jmp	xfer2
		
;*********************************************************
;* Send the initials                                     *
;*********************************************************
sendinit	ldx	#(initl/256d)
		ldy	#(initl&$ff)
		lda	#numit
		jmp	xfer2
		
		
;**********************************************************
		.sbttl "EEROM Xfer Utilities"
;**********************************************************
;* Xfer - Transfers bytes from the input port to a buffer *
;*                                                        *
;* Entry:	A = Number of bytes to xfer                   *
;*		X = MSB of pointer to buffer                  *
;*		Y = LSB of pointer to buffer                  *
;*                                                        *
;* Exit:	On Exit, and 'ack' is send back down the com  *
;*		port and the status of the program is         *
;*          returned to normal.                           *
;* 		X = 0 = Status Good                           *
;*		X = 2 = Timeout, All data not recieved        *
;**********************************************************
xfer		sta	datflg		;Save count total
		stx	r_nbuf+1,ABS	;Set up pointer
		sty	r_nbuf,ABS
		lda	#00
		sta	datnum		;Set to accept data, skip commands mode
		sta	r_nptr		;Set count to 0
		lda	#01			;ack ready to send
		sta	outdata
		cli				;Allow sounds to continue now
;Ready for Data in
		ldx	#04			;MSB of timeout
		ldy	#00			;Time out if this takes too long
?xf1		dey
		ifeq
			dex
			beq	timot			;Will send timout error
		endif
		lda	r_nptr		;See if we are getting anything
		cmp	datflg		;Did we get all?
		bcc	?xf1			;Wait and get some more
		ldx	#00			;X = 0 is good exit
		rts	
		
;********************************************************
;* If here, we timed out... Send back timeout and reset *
;********************************************************
timot		ldx	#g_tout
		stx	outdata
		rts	
		
;********************************************************
;* Routine for sending Data to Alpha                    *
;********************************************************
xfer2		sta	datflg		;Save count total
		stx	r_nbuf+1,ABS	;Set up pointer
		sty	r_nbuf,ABS
		tay				;Indirect pointer address
		dey				;One less (offset from 0)
		begin
			begin
				lda	#alpharcvd		;Test for output empty
				bit	portst
			neend				;Alphy ready to recieve
			lda	(r_nbuf,Y)
			sta	outdata		;Send data
			dey
		miend
		jmp	exit2			;We are done!

;***************************************************************************
;* Music Tables
;***************************************************************************		
		
	NEWTUNE(s_start)	
	.byte $82,$02
	.byte $80,$9D
	.byte $87,$06
	.byte $8E,$09
	.byte $4B,$20
	.byte $46,$20
	.byte $3F,$20
	.byte $46,$20
	.byte $4B,$20
	.byte $46,$20
	.byte $8F,$00
	.byte $4B,$40
	.byte $00,$80
	.byte $00,$00
		
	NEWTUNE(s_start2)	
	.byte $82,$03
	.byte $80,$9D
	.byte $87,$06
	.byte $37,$C0
	.byte $37,$C1
	.byte $35,$C0
	.byte $35,$C1
	.byte $33,$C0
	.byte $33,$C1
	.byte $35,$C0
	.byte $35,$41
	.byte $00,$60
	.byte $37,$20
	.byte $37,$C0
	.byte $37,$41
	.byte $00,$80
	.byte $00,$00
		
	NEWTUNE(s_start3)	
	.byte $82,$03
	.byte $80,$9D
	.byte $87,$06
	.byte $2E,$C0
	.byte $2E,$C1
	.byte $2C,$C0
	.byte $2C,$C1
	.byte $2A,$C0
	.byte $2A,$C1
	.byte $2C,$C0
	.byte $2C,$41
	.byte $00,$60
	.byte $2E,$20
	.byte $2E,$C0
	.byte $2E,$41
	.byte $00,$80
	.byte $00,$00
		
	NEWTUNE(s_start4)	
	.byte $82,$03
	.byte $80,$9D
	.byte $87,$06
	.byte $27,$C0
	.byte $27,$C1
	.byte $25,$C0
	.byte $25,$C1
	.byte $2F,$C0
	.byte $2F,$C1
	.byte $25,$C0
	.byte $25,$41
	.byte $00,$60
	.byte $27,$20
	.byte $27,$C0
	.byte $27,$41
	.byte $00,$80
	.byte $00,$00
		
	NEWTUNE(s_start5)	
	.byte $00,$10
	.byte $4B,$10
	.byte $4B,$10
	.byte $4B,$10
	.byte $83,$FF
	.byte $4B,$10
	.byte $4B,$10
	.byte $4B,$10
	.byte $4B,$10
	.byte $83,$01
	.byte $00,$00
		
;* Enter Maze Music - High Monotone
	NEWTUNE(s_mystery)	
	.byte $82,$01
	.byte $80,$34
	.byte $87,$06
	.byte $8E,$02
	.byte $00,$80
	.byte $8D,$56
	.byte $8F,$00
	.byte $00,$80
	.byte $00,$80
	.byte $8D,$56
	.byte $4B,$40,$00,$40
	.byte $00,$00

;* Enter Maze Music - Harmony High
	NEWTUNE(s_mystery2)	
	.byte $82,$01
	.byte $80,$34
	.byte $87,$06
	.byte $8E,$02
	.byte $3B,$80 
	.byte $3A,$40
	.byte $00,$40
	.byte $8F,$00
	.byte $39,$80
	.byte $38,$80
	.byte $3A,$C0
	.byte $00,$40
	.byte $00,$00

;* Enter Maze Music - Harmony Low		
	NEWTUNE(s_mystery3)	
	.byte $82,$01
	.byte $80,$34
	.byte $87,$06
	.byte $8E,$02
	.byte $38,$80
	.byte $37,$40
	.byte $00,$40
	.byte $8F,$00
	.byte $36,$80
	.byte $35,$80
	.byte $37,$C0
	.byte $00,$40
	.byte $00,$00

;* Enter Maze Music - Bass Line		
	NEWTUNE(s_mystery4)	
	.byte $82,$01
	.byte $80,$34
	.byte $87,$06
	.byte $8E,$07
	.byte $1B,$10
	.byte $00,$10
	.byte $27,$10
	.byte $00,$10
	.byte $26,$10
	.byte $00,$10
	.byte $1A,$10
	.byte $00,$10
	.byte $8F,$00
	.byte $1B,$40
	.byte $00,$40
	.byte $00,$00
		
	NEWTUNE(s_breakout)	
	.byte $82,$02
	.byte $80,$B1
	.byte $87,$06
	.byte $8E,$02
	.byte $42,$C0
	.byte $41,$60
	.byte $00,$40
	.byte $42,$20
	.byte $8F,$00
	.byte $42,$60
	.byte $00,$40
	.byte $41,$20
	.byte $41,$60
	.byte $00,$40
	.byte $42,$20
	.byte $3B,$C0
	.byte $3B,$C1
	.byte $4B,$C0
	.byte $00,$00
		
	NEWTUNE(s_breakout2)	
	.byte $82,$02
	.byte $80,$B1
	.byte $87,$06
	.byte $8E,$02
	.byte $3F,$C0
	.byte $3E,$60
	.byte $00,$40
	.byte $3E,$20
	.byte $8F,$00
	.byte $3F,$60
	.byte $00,$40
	.byte $3E,$20
	.byte $3E,$60
	.byte $00,$40
	.byte $3E,$20
	.byte $3F,$C0
	.byte $3F,$C1
	.byte $42,$C0
	.byte $00,$00
		
	NEWTUNE(s_breakout3)	
	.byte $82,$02
	.byte $80,$B1
	.byte $87,$06
	.byte $8E,$02
	.byte $47,$C0
	.byte $47,$60
	.byte $00,$40
	.byte $47,$20
	.byte $8F,$00
	.byte $47,$60
	.byte $00,$40
	.byte $47,$20
	.byte $47,$60
	.byte $00,$40
	.byte $47,$20
	.byte $47,$C0
	.byte $47,$C1
	.byte $53,$C0
	.byte $00,$00
		
	NEWTUNE(s_breakout4)	
	.byte $82,$02
	.byte $80,$B1
	.byte $87,$06
	.byte $8E,$03
	.byte $33,$20
	.byte $2F,$20
	.byte $33,$20
	.byte $36,$20
	.byte $33,$20
	.byte $2F,$20
	.byte $32,$20
	.byte $2F,$20
	.byte $32,$20
	.byte $35,$20
	.byte $32,$20
	.byte $2F,$20
	.byte $8F,$00
	.byte $2F,$20
	.byte $23,$20
	.byte $25,$20
	.byte $27,$20
	.byte $28,$20
	.byte $29,$20
	.byte $2A,$20
	.byte $29,$20
	.byte $2C,$20
	.byte $2A,$20
	.byte $2C,$20
	.byte $2E,$20
	.byte $2F,$C0
	.byte $00,$00
		
	NEWTUNE(s_escape)	
	.byte $82,$02
	.byte $87,$06
	.byte $80,$64
	.byte $8E,$10
	.byte $49,$80
	.byte $49,$81
	.byte $85,$01
	.byte $8F,$00
	.byte $00,$00
		
	NEWTUNE(s_escape2)	
	.byte $82,$02
	.byte $87,$06
	.byte $80,$64
	.byte $8E,$10
	.byte $00,$60
	.byte $00,$10
	.byte $36,$10
	.byte $36,$40
	.byte $31,$40
	.byte $85,$01
	.byte $8F,$00
	.byte $00,$00
		
	NEWTUNE(s_escape3)	
	.byte $82,$02
	.byte $87,$06
	.byte $80,$64
	.byte $8E,$10
	.byte $00,$60
	.byte $00,$10
	.byte $31,$10
	.byte $31,$40
	.byte $2E,$40
	.byte $85,$01
	.byte $8F,$00
	.byte $00,$00
		
	NEWTUNE(s_escape4)	
	.byte $82,$02
	.byte $87,$06
	.byte $80,$64
	.byte $8E,$10
	.byte $19,$10
	.byte $20,$10
	.byte $25,$10
	.byte $2C,$10
	.byte $29,$10
	.byte $25,$10
	.byte $29,$10
	.byte $2A,$10
	.byte $2C,$10
	.byte $2A,$10
	.byte $27,$10
	.byte $25,$10
	.byte $20,$10
	.byte $1E,$10
	.byte $1D,$10
	.byte $19,$10
	.byte $85,$01
	.byte $8F,$00
	.byte $00,$00
		
	NEWTUNE(s_highscore)	
	.byte $41,$60
	.byte $35,$20
	.byte $38,$20
	.byte $3D,$20
	.byte $41,$20
	.byte $00,$40
	.byte $3F,$40
	.byte $00,$20
	.byte $3D,$60
	.byte $3A,$20
	.byte $3D,$20
	.byte $41,$20
	.byte $46,$20
	.byte $00,$40
	.byte $49,$40
	.byte $00,$20
	.byte $00,$00
		
	NEWTUNE(s_highscore2)	
	.byte $83,$02
	.byte $35,$10
	.byte $36,$10
	.byte $35,$10
	.byte $33,$10
	.byte $31,$10
	.byte $30,$10
	.byte $83,$FE
	.byte $31,$20
	.byte $35,$20
	.byte $38,$20
	.byte $3D,$20
	.byte $00,$40
	.byte $3C,$40
	.byte $00,$20
	.byte $83,$02
	.byte $35,$10
	.byte $36,$10
	.byte $35,$10
	.byte $33,$10
	.byte $31,$10
	.byte $30,$10
	.byte $83,$FE
	.byte $31,$20
	.byte $35,$20
	.byte $3A,$20
	.byte $3D,$20
	.byte $00,$40
	.byte $37,$40
	.byte $00,$20
	.byte $00,$00
		
	NEWTUNE(s_highscore3)	
	.byte $2C,$60
	.byte $2C,$20
	.byte $2C,$20
	.byte $2C,$20
	.byte $2C,$20
	.byte $00,$40
	.byte $2D,$40
	.byte $00,$20
	.byte $2E,$60
	.byte $2E,$20
	.byte $2E,$20
	.byte $2E,$20
	.byte $2E,$20
	.byte $00,$40
	.byte $2E,$40
	.byte $00,$20
	.byte $00,$00
		
	NEWTUNE(s_highscore4)	
	.byte $25,$60
	.byte $25,$20
	.byte $25,$20
	.byte $25,$20
	.byte $25,$20
	.byte $00,$40
	.byte $24,$40
	.byte $00,$20
	.byte $22,$60
	.byte $22,$20
	.byte $22,$20
	.byte $22,$20
	.byte $22,$20
	.byte $00,$40
	.byte $1B,$40
	.byte $00,$20
	.byte $00,$00
		
	NEWTUNE(s_highscore5)	
	.byte $82,$02
	.byte $80,$91
	.byte $87,$06
	.byte $8D,$63
	.byte $47,$60
	.byte $47,$20
	.byte $44,$20
	.byte $40,$20
	.byte $3B,$40
	.byte $00,$20
	.byte $3B,$20
	.byte $34,$20
	.byte $38,$20
	.byte $8E,$05
	.byte $47,$10
	.byte $49,$10
	.byte $8F,$00
	.byte $47,$10
	.byte $46,$10
	.byte $47,$20
	.byte $00,$40
	.byte $44,$40
	.byte $00,$20
	.byte $8D,$63
	.byte $8E,$02
	.byte $49,$60
	.byte $49,$20
	.byte $47,$20
	.byte $45,$20
	.byte $8F,$00
	.byte $8E,$05
	.byte $49,$10
	.byte $4B,$10
	.byte $8F,$00
	.byte $49,$10
	.byte $48,$10
	.byte $49,$60
	.byte $00,$60
	.byte $00,$00
		
	NEWTUNE(s_highscore6)	
	.byte $82,$02
	.byte $80,$91
	.byte $87,$06
	.byte $8D,$64
	.byte $8E,$02
	.byte $83,$02
	.byte $38,$10
	.byte $39,$10
	.byte $38,$10
	.byte $36,$10
	.byte $34,$10
	.byte $33,$10
	.byte $83,$FE
	.byte $34,$20
	.byte $00,$40
	.byte $8F,$00
	.byte $83,$02
	.byte $36,$10
	.byte $38,$10
	.byte $39,$10
	.byte $38,$10
	.byte $36,$10
	.byte $34,$10
	.byte $33,$10
	.byte $31,$10
	.byte $33,$10
	.byte $34,$10
	.byte $36,$10
	.byte $34,$10
	.byte $83,$FE
	.byte $36,$20
	.byte $00,$40
	.byte $36,$40
	.byte $00,$20
	.byte $8D,$64
	.byte $40,$60
	.byte $40,$20
	.byte $40,$20
	.byte $40,$20
	.byte $3F,$60
	.byte $3F,$20
	.byte $3F,$20
	.byte $3F,$20
	.byte $38,$60
	.byte $38,$20
	.byte $38,$20
	.byte $38,$20
	.byte $3D,$60
	.byte $00,$60
	.byte $00,$00
		
	NEWTUNE(s_highscore7)	
	.byte $82,$02
	.byte $80,$91
	.byte $87,$06
	.byte $8D,$65
	.byte $40,$60
	.byte $40,$20
	.byte $3B,$20
	.byte $38,$20
	.byte $38,$40
	.byte $00,$20
	.byte $38,$20
	.byte $3B,$20
	.byte $40,$20
	.byte $3F,$60
	.byte $3F,$20
	.byte $3F,$20
	.byte $3F,$20
	.byte $3F,$20
	.byte $00,$40
	.byte $3C,$40
	.byte $00,$20
	.byte $8D,$65
	.byte $83,$02
	.byte $34,$10
	.byte $33,$10
	.byte $31,$10
	.byte $39,$10
	.byte $31,$10
	.byte $34,$10
	.byte $83,$FE
	.byte $39,$20
	.byte $00,$40
	.byte $83,$02
	.byte $36,$10
	.byte $34,$10
	.byte $33,$10
	.byte $2F,$10
	.byte $33,$10
	.byte $36,$10
	.byte $83,$FE
	.byte $39,$20
	.byte $00,$40
	.byte $83,$02
	.byte $35,$10
	.byte $36,$10
	.byte $35,$10
	.byte $33,$10
	.byte $31,$10
	.byte $33,$10
	.byte $83,$FE
	.byte $35,$20
	.byte $35,$20
	.byte $35,$20
	.byte $35,$60
	.byte $00,$60
	.byte $00,$00
		
	NEWTUNE(s_highscore8)	
	.byte $82,$02
	.byte $80,$91
	.byte $87,$06
	.byte $8D,$66
	.byte $8E,$03
	.byte $23,$60
	.byte $23,$20
	.byte $23,$20
	.byte $23,$20
	.byte $8F,$00
	.byte $23,$20
	.byte $00,$40
	.byte $20,$40
	.byte $00,$20
	.byte $8D,$66
	.byte $8E,$03
	.byte $21,$60
	.byte $21,$20
	.byte $21,$20
	.byte $21,$20
	.byte $85,$02
	.byte $8F,$00
	.byte $84,$00
	.byte $25,$60
	.byte $00,$60
	.byte $00,$00
		

;******************************************************
		.sbttl "EEROM Applications"
;******************************************************
;* If bit is set, that section is either read or      *
;* written depending of request byte                  *
;*	Bit 0 = Initials                                *
;*    Bit 1 = High Scores                             *
;*    Bit 2 = Stats and Numbers                       *
;*    Bit 3 = Option Switches                         *
;******************************************************
		
teax		.byte $02,$0B,$0C,$18,$19,$87
teasrl	.byte $95,$0F,$9F,$0F,$00,$0F

teasrh	= teasrl+1


eezopt	lda	#08			;Clear Options Only
		bne	genzer
eezboo	lda	#04			;Clear Bookkeeping Only
		bne	genzer
eezhis	lda	#03			;Clear High Scores and Initials
		bne	genzer
eezero	lda	#07			;Clear all areas
genzer	ldy	#$FF			;Request Clear Function
		bne	genreq
wropt		lda	#08			;Re-write Option switches
		bne	nozero
wrhs		lda	#01			;Re-write High Scores
		bne	nozero
wrini		lda	#02			;Re-write Initials
		bne	nozero
wrbook	lda	#04			;Re-write Bookkeeping
nozero	ldy	#00			
genreq	sty	eeflg			;Save request flag
		pha	
		ora	eerequ		;Put in request section flags
		sta	eerequ
		pla	
		ora	eerwrq		;Put in write bytes also
		sta	eerwrq
		rts	
rehiin	lda	#07			;Read in everything
		sta	eerequ		;Put in request
		lda	#00
		sta	eerwrq		;Clear write requests
		
;************** Fall through to handle requests ***********************
		.sbttl "EEROM I/O Mainline"
;**********************************************************************
;* Input:  eeflg: 00=no activity, 80=erase, 40=write, 20=read         *
;*	     eex:	Index into EEROM to access this data                *
;*         eebc:  Offset from eesrce of RAM data to access            *
;*         eecnt:	EEROM offset of last byte to modify                 *
;*                (stop when eex > eecnt)                             *
;*                                                                    *
;* Output:	EEROM erased, written or read                             *
;**********************************************************************
	
eeupd	lda	eeflg
		ifeq				;Not active
			lda	eerequ		;Any requests??
			ifne				;yep
				ldx	#00
				stx	eebc			;Source index
				stx	eecs			;Checksum
				stx	eesel			;Select code storage
				ldx	#08
				sec	
				begin				;Loop untill first bit is found
					ror	eesel			;Move bit about
					asl	A
					dex	
				csend				;Exit when set bit is found
				ldy	#eeras		;Guess Erase/Write
				lda	eesel			;What selected
				and	eerwrq		;Any writes requested??
				ifeq				;no, must be read
					ldy	#eeread
				endif
				sty	eeflg			;Save request
				lda	eesel
				eor	eerequ
				sta	eerequ		;Turn off request bit (ack)
				txa	
				asl	A
				tax	
				lda	teax,X		;Set up params for EEROM write
				sta	eex
				lda	teax+1,X
				sta	eecnt			;Save count (end)
				lda	teasrl,X
				sta	eesrce		;Source pointer
				lda	teasrh,X
				sta	eesrce+1
			endif
		endif
		lda	eeflg			;Anything to do??
		ifeq
			rts				;Nope, just leave
		endif
		ldy	eebc			;Yep, what to do?
		ldx	eex
		asl	A
		ifcs				;R, W or Erase?
			;Erase Request
			ldy	#eewrit
			sty	eeflg			;Signal busy with write, will clear on next pass
		else				;Read or Write
			ifmi				;Write a byte
				lda	#eeras
				sta	eeflg			;Signal writing a byte
				lda	eezflg		;Was this an erase request?
				ifne				;yep
					lda	#00
					sta	(eesrce,Y)		;Clear source too
				endif
				lda	(eesrce,Y)		;Get data to write
				cpx	eecnt			;Last one?
				ifcs				;yep
					lda	#00
					sta	eeflg			;We are done
					lda	eecs			;Get checksum
				endif
				sta	eerom,X		;Write to chip
				ldy	#eeras
			else				;Must be a read
				;Note: Can read all at once, no need to wait
				lda	eerom,X		;Get data
				cpx	eecnt			;Got all?
				ifcs				;yes
					eor	eecs			;Match checksum??
					ifne
						ldy	#-1
						lda	eesel
						ora	eebad
						sta	eebad
					endif
					lda	#00
					sta	eeflg			;One bad... All Bad!!
				else
					sta	(eesrce,Y)		;Copy to RAM
				endif
				ldy	#00
			endif
			clc	
			adc	eecs			;Update Checksum
			sta	eecs
			inc	eebc
			inc	eex
		endif
		tya
		ifeq				;Read??
			jmp	eeupd			;Do all reads at once
		endif
		rts	

;****************************************************
;* Sound Effects Table EFX2                         *
;****************************************************	
	NEWDATA2(t2_xlife,$59)
	.byte $02,$3C,$0E,$00
	.byte $02,$C4,$0E,$00
	.byte $FF,$04,$0B
	
	NEWDATA1(t1_xlife,$59)
	.byte $02,$60,$00,$9E,$00,$00

	NEWTUNE(s_xlife)	
	.byte $86,t1_xlife
	.byte $87,t2_xlife
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$A0
	.byte $00,$00
	
;*************************************
	NEWDATA2(t2_bonus,$5a)
	.byte $02,$10
	.byte $06,$00
	
	NEWDATA1(t1_bonus,$5a)
	.byte $02,$F0
	.byte $00,$06
	.byte $00,$00
		
	NEWTUNE(s_bonus)	
	.byte $86,t1_bonus
	.byte $87,t2_bonus
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$08
	.byte $00,$00
	
;*************************************
	NEWDATA2(t2_shield,$5b)
	.byte $02,$20,$02,$00,$02,$04,$FF,$06,$05
	
	NEWDATA1(t1_shield,$5b)
	.byte $02,$70,$00,$12,$00,$00
		
	NEWTUNE(s_shield)	
	.byte $86,t1_shield
	.byte $87,t2_shield
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$C0
	.byte $00,$12
	.byte $00,$00
	
;*************************************
	NEWDATA2(t2_manwall,$5c)
	.byte $02,$3C,$08,$00,$02,$F0,$08,$00,$02,$F8,$02,$00,$02,$F8,$FF,$04
	.byte $07,$1A,$00
	
	NEWDATA1(t1_manwall,$5c)
	.byte $0E,$00,$00,$02,$20,$01,$06,$00,$00,$02,$00,$FF,$04
	.byte $00,$00,$02,$20,$00,$04,$00,$00,$FF,$05,$09,$02,$30,$00,$04,$00,$00
			
	NEWTUNE(s_manwall)	
	.byte $86,t1_manwall
	.byte $87,t2_manwall
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$00
	.byte $00,$44
	.byte $00,$00
	
;*************************************
	NEWDATA2(t2_oxygen,$5d)
	.byte $02,$3C,$04,$00,$02,$FC,$FF,$08,$07,$02,$00,$02,$FC,$FF,$05,$07
	
	NEWDATA1(t1_oxygen,$5d)
	.byte $50,$00,$00
		
	NEWTUNE(s_oxygen)	
	.byte $86,t1_oxygen
	.byte $87,t2_oxygen
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$80
	.byte $00,$50
	.byte $00,$00
	
;*************************************
	NEWDATA2(t2_robshot,$5e)
	.byte $02,$3C,$02,$F8,$02,$CC,$02,$0C,$02,$00,$02,$FC,$02,$00,$02,$FC,$02,$00
	
	NEWDATA1(t1_robshot,$5e)
	.byte $02,$40,$01,$04,$00,$00,$02,$C0,$03,$0A,$00,$01
		
	NEWTUNE(s_robshot)	
	.byte $86,t1_robshot
	.byte $87,t2_robshot
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$12
	.byte $00,$00
	
;*************************************
	NEWDATA2(t2_footstep,$5f)
	.byte $02,$3C,$02,$C4
	
	NEWDATA1(t1_footstep,$5f)
	.byte $02,$10,$00,$02,$00,$00
		
	NEWTUNE(s_footstep)	
	.byte $86,t1_footstep
	.byte $87,t2_footstep
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$00
	.byte $00,$04
	.byte $00,$00
	
;*************************************
	NEWDATA2(t2_nooxy,$60)
	.byte $02,$20,$26,$00,$02,$E0,$26,$00,$FF,$02,$0B
	
	NEWDATA1(t1_nooxy,$60)
	.byte $02,$40,$01,$EE,$00,$00
		
	NEWTUNE(s_nooxy)	
	.byte $86,t1_nooxy
	.byte $87,t2_nooxy
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$F0
	.byte $00,$00

;*************************************
	NEWDATA2(t2_bassboom,$61)
	NEWDATA1(t1_bassboom,$61)
	NEWTUNE(s_bassboom)	
	.byte $00,$00

;*************************************
	NEWDATA2(t2_bassboom2,$62)
	NEWDATA1(t1_bassboom2,$62)
	NEWTUNE(s_bassboom2)	
	.byte $00,$00
	
;*************************************
	NEWDATA2(t2_alert2,$63)
	.byte $02,$10
	.byte $0E,$00
	.byte $02,$04
	.byte $04,$08
	.byte $02,$04
	.byte $04,$08
	.byte $02,$04
	.byte $18,$00
	.byte $16,$FC
	.byte $08,$00
	
	NEWDATA1(t1_alert2,$63)
	.byte $02,$C0
	.byte $03,$52
	.byte $00,$00

	NEWTUNE(s_alert2)	
	.byte $8B,$01
	.byte $86,t1_alert2
	.byte $87,t2_alert2
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$C0
	.byte $00,$54
	.byte $00,$00
	
;*************************************
	NEWDATA2(t2_redalert,$64)
	.byte $02,$24,$56,$00
	
	NEWDATA1(t1_redalert,$64)
	.byte $02,$40,$07,$02,$00,$00,$02
	.byte $00,$FE,$04,$00,$00,$02,$F0,$01,$04,$00,$00,$02,$10,$FE,$04,$00
	.byte $00,$02,$E0,$01,$04,$00,$00,$02,$20,$FE,$04,$00,$00,$02,$E0,$01
	.byte $04,$00,$00,$FF,$04,$0F
		
	NEWTUNE(s_redalert)	
	.byte $86,t1_redalert
	.byte $87,t2_redalert
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$58
	.byte $00,$00
	
;*************************************
	NEWDATA2(t2_ataritune,$65)
	NEWDATA1(t1_ataritune,$65)
	NEWTUNE(s_ataritune)	
	.byte $00,$00

;*************************************
	NEWDATA2(t2_ataritune2,$66)
	NEWDATA1(t1_ataritune2,$66)
	NEWTUNE(s_ataritune2)	
	.byte $00,$00
	
;*************************************
	NEWDATA2(t2_beeps,$67)
	.byte $02,$10
	.byte $06,$00
	.byte $02,$F0
	.byte $02,$00
	.byte $FF,$4A
	.byte $0B
	
	NEWDATA1(t1_beeps,$67)
	.byte $02,$F0
	.byte $00,$0A
	.byte $00,$00
	.byte $0C,$00
	.byte $00,$FF
	.byte $4A,$06
		
	NEWTUNE(s_beeps)	
	.byte $86,t1_beeps
	.byte $87,t2_beeps
	.byte $90,$00
	.byte $82,$00
	.byte $8A,$A0
	.byte $00,$8A
	.byte $8A,$A0
	.byte $00,$FF
	.byte $8A,$A0
	.byte $00,$FF
	.byte $8A,$A0
	.byte $00,$FF
	.byte $00,$00
	
;*************************************	
;* System Startup Pointers           *
;*************************************
	.org $bffa
	
	.word		nmi
	.word		pwron
	.word 	irq
		
	.end


