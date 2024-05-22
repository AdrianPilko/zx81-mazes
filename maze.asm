; Copyright (c) 2024 Adrian Pilkington

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

;;; Maze game
;;;
;;; https://youtube.com/@byteforever7829

;;; Known bug(s)

;pasmo only accepts DEFINE


CLS EQU $0A2A
PRINTAT EQU $08f5
PRINT EQU $10


KEYBOARD_READ_PORT_P_TO_Y	EQU $DF
; for start key
KEYBOARD_READ_PORT_A_TO_G	EQU $FD
; keyboard port for shift key to v
KEYBOARD_READ_PORT_SHIFT_TO_V EQU $FE
; keyboard space to b
KEYBOARD_READ_PORT_SPACE_TO_B EQU $7F
; keyboard q to t
KEYBOARD_READ_PORT_Q_TO_T EQU $FB

; starting port numbner for keyboard, is same as first port for shift to v
KEYBOARD_READ_PORT EQU $FE
SCREEN_WIDTH EQU 32
SCREEN_HEIGHT EQU 23   ; we can use the full screen becuase we're not using PRINT or PRINT AT ROM subroutines
MISSILE_COUNTDOWN_INIT EQU 18
;#define PLAYER_START_POS 604
PLAYER_START_POS EQU 637
PLAYER_LIVES EQU 3
;PIRATE_START_POS EQU 366
PIRATE_START_POS EQU 36
LEVEL_COUNT_DOWN_INIT EQU 4
LEV_COUNTDOWN_TO_INVOKE_BOSS EQU 1

VSYNCLOOP       EQU      1

; character set definition/helpers
__:				EQU	$00	;spacja
_QT:			EQU	$0B	;"
_PD:			EQU	$0C	;funt
_SD:			EQU	$0D	;$
_CL:			EQU	$0E	;:
_QM:			EQU	$0F	;?
_OP:			EQU	$10	;(
_CP:			EQU	$11	;)
_GT:			EQU	$12	;>
_LT:			EQU	$13	;<
_EQ:			EQU	$14	;=
_PL:			EQU	$15	;+
_MI:			EQU	$16	;-
_AS:			EQU	$17	;*
_SL:			EQU	$18	;/
_SC:			EQU	$19	;;
_CM:			EQU	$1A	;,
_DT:			EQU	$1B	;.
_NL:			EQU	$76	;NEWLINE

_BL             EQU $80; solid block

_0				EQU $1C
_1				EQU $1D
_2				EQU $1E
_3				EQU $1F
_4				EQU $20
_5				EQU $21
_6				EQU $22
_7				EQU $23
_8				EQU $24
_9				EQU $25
_A				EQU $26
_B				EQU $27
_C				EQU $28
_D				EQU $29
_E				EQU $2A
_F				EQU $2B
_G				EQU $2C
_H				EQU $2D
_I				EQU $2E
_J				EQU $2F
_K				EQU $30
_L				EQU $31
_M				EQU $32
_N				EQU $33
_O				EQU $34
_P				EQU $35
_Q				EQU $36
_R				EQU $37
_S				EQU $38
_T				EQU $39
_U				EQU $3A
_V				EQU $3B
_W				EQU $3C
_X				EQU $3D
_Y				EQU $3E
_Z				EQU $3F


;;;; this is the whole ZX81 runtime system and gets assembled and
;;;; loads as it would if we just powered/booted into basic

           ORG  $4009             ; assemble to this address

VERSN
    DB 0
E_PPC:
    DW 2
D_FILE:
    DW Display
DF_CC:
    DW Display+1                  ; First character of display
VARS:
    DW Variables
DEST:           DW 0
E_LINE:         DW BasicEnd
CH_ADD:         DW BasicEnd+4                 ; Simulate SAVE "X"
X_PTR:          DW 0
STKBOT:         DW BasicEnd+5
STKEND:         DW BasicEnd+5                 ; Empty stack
BREG:           DB 0
MEM:            DW MEMBOT
UNUSED1:        DB 0
DF_SZ:          DB 2
S_TOP:          DW $0002                      ; Top program line number
LAST_K:         DW $fdbf
DEBOUN:         DB 15
MARGIN:         DB 55
NXTLIN:         DW Line2                      ; Next line address
OLDPPC:         DW 0
FLAGX:          DB 0
STRLEN:         DW 0
T_ADDR:         DW $0c8d
SEED:           DW 0
FRAMES:         DW $f5a3
COORDS:         DW 0
PR_CC:          DB $bc
S_POSN:         DW $1821
CDFLAG:         DB $40
MEMBOT:         DB 0,0 ;  zeros
UNUNSED2:       DW 0

            ORG 16509       ;; we have to push the place in memory for this here becuase basic has
                    ;; to start at 16514 if memory was tight we could use the space between UNUSED2
                    ;; and Line1 for variables

Line1:          DB $00,$0a                    ; Line 10
                DW Line1End-Line1Text         ; Line 10 length
Line1Text:      DB $ea                        ; REM

start

introWaitLoop
	ld b,64
introWaitLoop_1
    push bc
    pop bc
	djnz introWaitLoop_1
    jp read_start_key     ;; have to have 2 labels as not a call return

read_start_key
	ld a, KEYBOARD_READ_PORT_A_TO_G
	in a, (KEYBOARD_READ_PORT)					; read from io port
	bit 1, a									; check S key pressed
	jp nz, introWaitLoop
    jr preinit  ; not really necessary

preinit
    ld a, 20
    ld (genRow), a
    ld a, 1
    ld (genCol), a

	call CLS  ; clears screen and sets the boarder
    ld bc, 1
    ld de, MAZE_TEXT
    call printstring
    call fillScreenBlack

genLoop
    ld b,VSYNCLOOP
waitForTVSync
    call vsync
    djnz waitForTVSync


	;call copyFromScrBuffToDisplayMem
	;ret


    ; using the binary tree method
    ; we start maze gen in the bottom left corner

    ;; we want to set the current location to blank
    ld a, (genCol)
    ld c, a
    ld a, (genRow)	 ; col set for PRINTAT
    ld b, a
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e
    ld a,0
    call PRINT

    ;generate a random number zero or one
    call setRandomNumberZeroOne
    cp 0

    jp z, setBlankAbove
    ;; else the column to the right blank
    ld a, (genCol)
    inc a
    ld c, a
    ld a, (genRow)
    ld b, a
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e
    ld a,0
    call PRINT

    jr checkGenColRow

setBlankAbove
    ld a,(genRow)
    dec a
    ld b, a
    ld a, (genCol)	 ; col set for PRINTAT
    ld c, a
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e
    ld a,0
    call PRINT

    ; in this case sometimes set the row above that to blank (to avoid having closed loops)
    ;; only do this if we're 3 away from top row
    call setRandomNumberZeroOne
    cp 1
    jp z, checkGenColRow

    ld a,(genRow)
    cp 3
    jp z, checkGenColRow
    cp 4
    jp z, checkGenColRow
    dec a
    dec a
    ld b, a
    ld a, (genCol)	 ; col set for PRINTAT
    ld c, a
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e
    ld a,0
    call PRINT


checkGenColRow
    ld a, (genCol)
    inc a          ; skip to next column
    inc a
    ld (genCol), a ; store the next column on

    cp 31
    jr z, resetGenColAndDec
    cp 32
    jr z, resetGenColAndDec
    ld (genCol),a
    jp genLoop
resetGenColAndDec
    ld a, 1
    ld (genCol),a
    ld a,(genRow)
    dec a
    dec a    ; jump 2 row up as we only set as if a grid not every row
    cp 0
    jr z, endOfGen
    cp 1
    jr z, endOfGen
    ld (genRow),a
    jp genLoop
endOfGen

    ; now set the entry point to the maze row 21
    ;; we want to set the current location to blank
    ld a, 0
    ld c, a  ; col for PRINTAT
    ld a, 20 ; row set for PRINTAT
    ld b, a
    call PRINTAT		; ROM routine to set current cursor position, from row b and column e
    ld a,0
    call PRINT

    call waitABit
    jp preinit
    ret



fillScreenBlack
    ld hl,Display+1
    ld de, 33
    add hl, de
    ld a, 128
    ld b, 21
rowLoop
    push bc
    ld b, 32
colLoop
    ld (hl),a
    inc hl
    djnz colLoop
    inc hl
    pop bc
    djnz rowLoop
    ret

copyFromScrBuffToDisplayMem
	ld b, 21
	ld hl, mazeScreenBuffer  ; has to be 32 * 21
	ld de, Display+34
copyScrBuffLoop
    push bc
	ld bc, 32
	ldir
	inc de ; inc de to next line because of $76 at end of line
	pop bc
	djnz copyScrBuffLoop
	ret



INCLUDE commonUtils.asm

                DB $76                        ; Newline
Line1End
Line2			DB $00,$14
                DW Line2End-Line2Text
Line2Text     	DB $F9,$D4                    ; RAND USR
				DB $1D,$22,$21,$1D,$20        ; 16514
                DB $7E                        ; Number
                DB $8F,$01,$04,$00,$00        ; Numeric encoding
                DB $76                        ; Newline
Line2End
endBasic

Display        	DB $76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, _P, _R, _E, _S, _S, 0, _S,0, _T, _O, 0, _S, _T, _A, _R, _T, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76

Variables

genCol
    DB 0
genRow
    DB 0
mazeVisitedLocations
    DS 32*21, 0
mazeScreenBuffer
    DS 32*21, 8

testSring
    DB _H,_E,_L,_L,_O,_CM,__,_W,_O,_R,_L,_D,$ff
MAZE_TEXT
    DB _M,_A,_Z,_E,__,_G,_E,_N,_E,_R,_A,_T,_I,_O,_N,__,_B,_Y,_T,_E,_F,_O,_R,_E,_V,_E,_R,$FF

VariablesEnd:   DB $80
BasicEnd:


