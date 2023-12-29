/*
	Initialization code and main loop
*/

INCLUDE "hardware.inc"
INCLUDE "memory.asm"
INCLUDE "ibmpc1.inc"

def CellSizeBytes equ 2


MACRO StoreCellLiteralHRAM
	; \1 is the location, \2 the value
	ld a, \2 & $FF
	ldh [\1], a
	ld a, \2 >> 8
	ldh [\1 + 1], a
ENDM

MACRO StoreCellLiteralWRAM
	; \1 is the location, \2 the value
	ld a, \2 & $FF
	ld [\1], a
	ld a, \2 >> 8
	ld [\1 + 1], a
ENDM

MACRO StoreCellFromRegisterPairHRAM
	; \1 is the src register low, \2 is the src register high \3 the src address in hram
	ld a, \1
	ldh [\3], a
	ld a, \2
	ldh [\3 + 1], a
ENDM

MACRO StoreCellFromRegisterPairWRAM
	; \1 is the src register low, \2 is the src register high \3 the src address in hram
	ld a, \1
	ld [\3], a
	ld a, \2
	ld [\3 + 1], a
ENDM

MACRO LoadCellHRAM
	; \1 is the dest register low, \2 is the dest register high \3 the src address in hram
	ldh a, [\3]
	ld \1, a
	ldh a, [\3 + 1]
	ld \2, a
ENDM

MACRO LoadCellWRAM
	; \1 is the dest register low, \2 is the dest register high \3 the src address in hram
	ld a, [\3]
	ld \1, a
	ld a, [\3 + 1]
	ld \2, a
ENDM

MACRO LoadCellFromHLHRAM
	; load the cell pointed to by HL. preserves value of hl
	; \1 is dest register low, \2 is dest register high
	push hl
		ld a, [hli]
		ld \1, a
		ld a, [hl]
		ld \2, a
	pop hl
ENDM

MACRO Cell
	ds CellSizeBytes
ENDM

MACRO MPopReturn
	; pop from return stack, returning values into a register pair
	; \1 low register, \1 high register
	LoadCellHRAM l, h, ReturnStackPtr
	dec hl
	dec hl
	LoadCellFromHLHRAM \1, \2
	StoreCellFromRegisterPairHRAM l, h, ReturnStackPtr
ENDM

MACRO MPushReturn
	; push data in register pair to return stack (low reg)\1,  (hi reg)\2
	LoadCellHRAM l, h, ReturnStackPtr
	ld a, \1
	ld [hli], a
	ld a, \2
	ld [hli], a
	StoreCellFromRegisterPairHRAM l, h, ReturnStackPtr
ENDM

MACRO MPopData
	; pop from data stack, returning values into a register pair
	; \1 low register, \1 high register
	LoadCellHRAM l, h, StackPtr
	dec hl
	dec hl
	LoadCellFromHLHRAM \1, \2
	StoreCellFromRegisterPairHRAM l, h, StackPtr
ENDM

MACRO MPushData
	; push data in register pair (low reg)\1,  (hi reg)\2
	LoadCellHRAM l, h, StackPtr
	ld a, \1
	ld [hli], a
	ld a, \2
	ld [hli], a
	StoreCellFromRegisterPairHRAM l, h, StackPtr
ENDM

MACRO ReadCellLiteral
	; read next address in stream as a cell literal into registers
	;/1 low dest register, /2 high dest register
	LoadCellHRAM l, h, IPtr
	ld a, [hli]
	ld \1, a
	ld a, [hli]
	ld \2, a
	StoreCellFromRegisterPairHRAM l, h, IPtr
ENDM

MACRO PeekCellLiteral
	;/1 low dest register, /2 high dest register
	push hl
		LoadCellHRAM l, h, IPtr
		ld a, [hli]
		ld \1, a
		ld a, [hli]
		ld \2, a
	pop hl
ENDM

MACRO MMultiply
	;; multiply DE and BC
;; DE is equivalent to the number in the top row in our algorithm
;; and BC is equivalent to the number in the bottom row in our algorithm

    ld a,16     ; this is the number of bits of the number to process
    ld hl,0     ; HL is updated with the partial result, and at the end it will hold 
                ; the final result.
.mul_loop
    srl b
    rr c        ;; divide BC by 2 and shifting the state of bit 0 into the carry
                ;; if carry = 0, then state of bit 0 was 0, (the rightmost digit was 0) 
                ;; if carry = 1, then state of bit 1 was 1. (the rightmost digit was 1)
                ;; if rightmost digit was 0, then the result would be 0, and we do the add.
                ;; if rightmost digit was 1, then the result is DE and we do the add.
    jr nc,.no_add	

    ;; will get to here if carry = 1        
    add hl,de   

.no_add
    ;; at this point BC has already been divided by 2

    ;ex de,hl    ;; swap DE and HL
    push bc
    ld b, d
    ld c, e
    ld d, h
    ld e, l
    ld h, b
    ld l, c
    add hl,hl   ;; multiply DE by 2
    ;ex de,hl    ;; swap DE and HL
    ld b, d
    ld c, e
    ld d, h
    ld e, l
    ld h, b
    ld l, c
    pop bc
    ;; at this point DE has been multiplied by 2
    
    dec a
    jr nz,.mul_loop  ;; process more bits
ENDM

MACRO MDivide
; 16 bit division
; DE = DE / BC, BC = remainder

;div_DE_BC_DEBCu:
        ld      hl,_MD16temp
        ld      [hl],c
        inc     hl
        ld      [hl],b
        inc     hl
        ld      [hl],17
        ld      bc,0
.nxtbit:
        ld      hl,_MD16count
        ld      a,e
        rla
        ld      e,a
        ld      a,d
        rla
        ld      d,a
        dec     [hl]
        ret     z
        ld      a,c
        rla
        ld      c,a
        ld      a,b
        rla
        ld      b,a
        dec     hl
        dec     hl
        ld      a,c
        sub     [hl]
        ld      c,a
        inc     hl
        ld      a,b
        sbc     a,[hl]
        ld      b,a
        jr      nc,.noadd

        dec     hl
        ld      a,c
        add     a,[hl]
        ld      c,a
        inc     hl
        ld      a,b
        adc     a,[hl]
        ld      b,a
.noadd:
        ccf
        jr      .nxtbit
ENDM

def WordNameLengthOffset equ 0
def WordNextOffset equ 1
def WordNameOffset equ 3
MACRO WordHeader
	; \1 word name, \2 next \3 internal name (WordNameSize chars exactly in length) \4 name length 
	
	\1:
	.nameLength: db \4
	.next: dw \2
	.name: db "\3"
	
	.code: dw .body
	.body:
ENDM

MACRO SecondaryWordHeader
	; \1 word name, \2 next 
	WordHeader \1, \2
		LoadCellHRAM c, b, IPtr
		MPushReturn c, b
		ld bc, thread
		StoreCellFromRegisterPairHRAM c, b, IPtr
		jp Top
	.thread:
ENDM

SECTION	"Vblank",ROM0[$0040]
	di
	call VBlank_ISR
	reti

SECTION	"stat",ROM0[$0048]
	reti

SECTION	"timer",ROM0[$0050]
	reti
SECTION "serial", ROM0[$0058]
	di
	call Serial_ISR
	reti

SECTION "joypad", ROM0[$0060]
	reti

SECTION "Header", ROM0[$100]

	jp EntryPoint

	ds $150 - @, 0 ; Make room for the header

def DataStackSize equ 16 * CellSizeBytes
def ReturnStackSize equ 16 * CellSizeBytes
def GameboyScreenWidthTiles equ 20
def GameboyScreenHeightTiles equ 18
def TerminalCursorKey equ $db
def CursorTimerOverflowAt equ 30 ; toggle after this many vblanks
def BackspaceAsciiVal equ $08
SECTION "Console out buffer", WRAM0
	def ShiftUpAddressValue equ 1
	def CVRAMChangeStructSize equ 3
	def COutQueueMaxSize equ 64 * CVRAMChangeStructSize
	def CShiftUpBufferMaxSize equ 32
	COutQueue: ds COutQueueMaxSize
	CShiftUpBuffer: ds CShiftUpBufferMaxSize
	CShiftUpBuffer2: ds CShiftUpBufferMaxSize
	CShiftUpBufferPopulated: db
	COutQueueEnd:
	COutQueueEndPtr: Cell
	COutQueueStartPtr: Cell
	COutQueueCurrentSize: Cell
	CHasOutgrownScreenFlag: db
	CCursorHoverValue: db
	CCursorBlinkTimer: db
	CGetCharFlag: db


SECTION "interpreter variables", HRAM
	_MD16temp:    ds 2
	_MD16count:   db
	ConsoleOnRow: db
	ConsoleOnColumn: db
	StackPtr: Cell
	IPtr: Cell
	ReturnStackPtr: Cell
	Stack: ds DataStackSize
	ReturnStack: ds ReturnStackSize

SECTION "main", ROM0

BlankCurrentRow:
	;d: row
	;e: col
	push de
	push bc
	call GetVRAMAddressInHL
	ld a, $20
	ld c, GameboyScreenWidthTiles
.loop:
	ld [hli], a
	dec c
	ld b, a
	ld a, 0
	cp a, c
	ld a, b
	jp nz, .loop
	pop bc
	pop de
	ret

EnqueueShiftUpVRAMChange:
	; a: which row should be blanked when the shift happens
	; enqueue a change that 
	; results in all lines being shifted up by one
	; with the top one being lost
	ld b, a
	ld de, ShiftUpAddressValue
	call EnqueueVRAMChange
	ret

EnqueueVRAMChange:
	; b: new value
	; de: address
	; returns:
	; b: mas size reached
	push hl
	LoadCellWRAM l, h, COutQueueCurrentSize
	ld a, l
	cp a, COutQueueMaxSize & $FF
	jp z, .lowByteMatch
	jp .noMatch
.lowByteMatch:
	ld a, h
	cp a, COutQueueMaxSize >> 7
	jp z, .maxSizeReached
.noMatch:
	inc hl
	StoreCellFromRegisterPairWRAM l, h, COutQueueCurrentSize
	LoadCellWRAM l, h, COutQueueEndPtr
	ld a, b
	ld [hli], a
	ld a, e
	ld [hli], a
	ld a, d
	ld [hli], a

	ld a, h
	cp a, (COutQueue + COutQueueMaxSize) >> 8
	jp z, .highIsEqual
	jp nc, .endOvershot
	jp .endNotOvershot
.highIsEqual:
	ld a, l
	cp a, (COutQueue + COutQueueMaxSize) & $ff
	jp z, .endOvershot
	jp nc, .endOvershot
	jp .endNotOvershot
.endOvershot:
	ld hl, COutQueue
.endNotOvershot:
	StoreCellFromRegisterPairWRAM l, h, COutQueueEndPtr
	jp .successfullyAdded
.maxSizeReached:
	ld b, 1
	jp .functionEnd
.successfullyAdded:
	ld b, 0
.functionEnd:
	pop hl
	ret

DequeueVRAMChange:
	; returns:
	; b: new value
	; c: is queue empty (1=yes, 0 = no)
	; de: address ( d is 0 if none ine queue)
	LoadCellWRAM l, h, COutQueueCurrentSize
	ld a, l
	cp a, 0
	jp z, .lowZero
	jp .notZero
.lowZero:
	ld a, h
	cp a, 0
	jp z, .queueEmpty
.notZero:
	dec hl
	StoreCellFromRegisterPairWRAM l, h, COutQueueCurrentSize
	LoadCellWRAM l, h, COutQueueStartPtr
	ld a, [hli]
	ld b, a
	ld a, [hli]
	ld e, a
	ld a, [hli]
	ld d, a

	ld a, h
	cp a, (COutQueue + COutQueueMaxSize) >> 8
	jp z, .highIsEqual
	jp nc, .endOvershot
	jp .endNotOvershot
.highIsEqual:
	ld a, l
	cp a, (COutQueue + COutQueueMaxSize) & $ff
	jp z, .endOvershot
	jp nc, .endOvershot
	jp .endNotOvershot
.endOvershot:
	ld hl, COutQueue
.endNotOvershot:
	StoreCellFromRegisterPairWRAM l, h, COutQueueStartPtr
	jp .successfullyAdded
.queueEmpty:
	ld c, 1 
	jp .end
.successfullyAdded:
	ld c, 0
.end:
	ret


Title:
    DB      "Hello World !"

DrawCursor:
	ld a, [CCursorBlinkTimer]
	inc a
	ld [CCursorBlinkTimer], a
	cp a, CursorTimerOverflowAt
	jp nz, .end
	; toggle the cursor
	ld a, [ConsoleOnRow]
	ld d, a
	ld a, [ConsoleOnColumn]
	ld e, a
	call GetVRAMAddressInHL
	ld a, [hl]
	ld e, a
	ld a, [CCursorHoverValue]
	ld [hl], a
	ld a, e
	ld [CCursorHoverValue], a
	ld a, 0
	ld [CCursorBlinkTimer], a
.end:
	ret

VBlank_ISR:	
	push bc
	push de
	push hl
	push af
	ld a, [rLY]
	cp a, 144
	jp nz, .endFunction
.loop:
	
	call DequeueVRAMChange
	ld h, d
	ld l, e
	ld a, c
	cp a, 1
	jp z, .endLoop
	ld a, l
	cp a, ShiftUpAddressValue & $ff
	jp z, .lowMatch
	jp .noMatch
.lowMatch:
	ld a, h
	cp a, ShiftUpAddressValue >> 8
	jp z, .highMatch
	jp .noMatch
.highMatch:
	; we need to shift all characters up by one row
	
	call ShiftUp
	ld d, b
	ld e, 0
	call BlankCurrentRow
	jp .endLoop
.noMatch:
	ld [hl], b
.endLoop:
.endFunction:
	call DrawCursor
	pop af
	pop hl
	pop de
	pop bc
	ret

ShiftUp:
	push af
	ld a, 1
	ld [CHasOutgrownScreenFlag], a
	ld a, [rSCY]
	add a, 8
	ld [rSCY], a
	cp a, $78
	jp z, .wrapRound
	jp .noWrap
.wrapRound:
	;ld a, 0
	;ld [ConsoleOnRow], a
.noWrap:
	pop af
	ret


Serial_ISR:
	push af
	push bc
	ld a, [CGetCharFlag]
	cp a, 1
	jp z, .getCFlagSet
	jp .getCFlagNotSet
.getCFlagSet:
	ld a, 0
	ld [CGetCharFlag], a
.getCFlagNotSet:
	ld a, $80
	ld [rSC], a
	pop bc
	pop af
	ret



TileData: 
    chr_IBMPC1      1,8 ; font

; called at the end of every primitive word, advances the instruction pointer and jumps to next word
Top:
	LoadCellHRAM l, h, IPtr
	ld a, [hli]
	ld c, a
	ld a, [hli]
	ld b, a
	StoreCellFromRegisterPairHRAM l, h, IPtr
	ld h, b
	ld l, c
	jp hl

StopLCD: 
    ld      a,[rLCDC]
    rlca                    ; Put the high bit of LCDC into the Carry flag
    ret     nc              ; Screen is off already. Exit.

	; Loop until we are in VBlank

.wait:
    ld      a,[rLY]
    cp      145             ; Is display on scan line 145 yet?
    jr      nz,.wait        ; no, keep waiting

	; Turn off the LCD

    ld      a,[rLCDC]
    res     7,a             ; Reset bit 7 of LCDC
    ld      [rLCDC],a

    ret

Init:
	; Shut down audio circuitry
    ld a, 0
    ld [rNR52], a

    call ConsoleInit
    call StopLCD

    ld a, IEF_SERIAL | IEF_VBLANK
    ld [rIE], a

    ; serial - set as slave
    ld a, $80
	ld [rSC], a
	ld a, 1
	ld [rSB], a


	StoreCellLiteralHRAM StackPtr, Stack
	StoreCellLiteralHRAM ReturnStackPtr, ReturnStack
	
	;  Here we are going to setup the background tile
	; palette so that the tiles appear in the proper
	; shades of grey.

    ld      a,$e4
    ld      [rBGP],a        ; Setup the default background palette

	;  Here we are setting the X/Y scroll registers
	; for the tile background to 0 so that we can see
	; the upper left corner of the tile background.

    ld      a,0
    ld      [rSCX],a
    ld      [rSCY],a

	;  For the purposes of the 'mem_CopyMono' routine,
	; the 16-bit HL register is used as a source memory
	; location, DE is used as a destination memory location,
	; and BC is used as a data length indicator.

    ld      hl,TileData
    ld      de,$8000
    ld      bc,8*256        ; length (8 bytes per tile) x (256 tiles)
    call    mem_CopyMono    ; Copy tile data to memory

    ; Next, we clear our 'canvas' to all white by
	; 'setting' the canvas to ascii character $20
	; which is a white space.

    ld      a,$20           ; Clear tile map memory
    ld      hl,$9800
    ld      bc,SCRN_VX_B * SCRN_VY_B
    call    mem_Set


	

    ld      a,LCDCF_ON|LCDCF_BG8000|LCDCF_BG9800|LCDCF_BGON|LCDCF_OBJ16|LCDCF_OBJOFF
    ld      [rLCDC],a       ; Turn screen on

	; Since we have accomplished our goal, we now have nothing
	; else to do. As a result, we just Jump to a label that
	; causes an infinite loop condition to occur.
	ret
; CONSOLE FUNCTIONS START
ConsoleInit:
	StoreCellLiteralHRAM ConsoleOnRow, 0
	StoreCellLiteralHRAM ConsoleOnColumn, 0
	StoreCellLiteralWRAM COutQueueEndPtr, COutQueue
	StoreCellLiteralWRAM COutQueueStartPtr, COutQueue
	StoreCellLiteralWRAM COutQueueCurrentSize, 0

	LoadCellWRAM l, h, COutQueueStartPtr
	ld bc, COutQueueMaxSize
	ld a, 0
	call mem_Set
	ld a, 0
	ld [CHasOutgrownScreenFlag], a
	ld a, TerminalCursorKey
	ld [CCursorHoverValue], a
	ld a, 0
	ld [CCursorBlinkTimer], a
	ld a, 0
	ld [CGetCharFlag], a
	ret
OnRowOverflow:
	ld e, 0
	inc d
	ld a, d
	ld a, GameboyScreenHeightTiles
	ret
OnColOverflow:
	dec d
	ret

GetVRAMAddressInHL:
	; d: row
	; e: col
	; hl: addressToWriteTo
	;ld      de,$9800+3+(SCRN_VY_B*7)
	push de
	push bc
	push de
	ld e, d
	ld d, 0
	ld b, 0
	ld c, SCRN_VY_B
	MMultiply ; mul de and bc, store in hl
	ld bc, _SCRN0 ; $9800
	add hl, bc
	pop de
	ld d, 0
	add hl, de
	pop bc
	;ld [hl], b
	pop de
	ret

EnqueueBlankCurrentCursor:
	; blank where the cursor was
	; to prevent the cursor being left behind
	ldh a, [ConsoleOnRow]
	ld d, a
	ldh a, [ConsoleOnColumn]
	ld e, a
	call GetVRAMAddressInHL
	ld e, l
	ld d, h
	ld b, $20
	call EnqueueVRAMChange
	ret

NewLine:
	; a: 1 if should skip blanking cursor
	push bc
	push de
	push hl
	cp a, 1
	jp z, .skipBlankingCursor
	call EnqueueBlankCurrentCursor
.skipBlankingCursor:
	;.. continue with rest of function
	ldh a, [ConsoleOnRow]
	inc a
	ld b, a
	cp a, GameboyScreenHeightTiles
	jp z, .heightOverflow
	jp nc, .heightOverflow
	jp c, .lessThan
	jp nz, .noHeightOverflow
.lessThan:
	ld a, [CHasOutgrownScreenFlag]
	cp a, 1
	ld a, b
	jp nz, .noHeightOverflow 
.heightOverflow:
	;dec a
	cp a, 32
	jp z, .wrapRound
	jp .noWrap
.wrapRound:
	ld a, 0
.noWrap
	push af
	call EnqueueShiftUpVRAMChange
	pop af
.noHeightOverflow:
	ld [ConsoleOnRow], a
	ld a, 0
	ld [ConsoleOnColumn], a
	pop hl
	pop de
	pop bc
	ret

AdvanceCursor:
	ldh a, [ConsoleOnColumn]
	inc a
	cp a, GameboyScreenWidthTiles
	jp z, .widthOverflow
	jp .noWidthOverflow
.widthOverflow:
	ld a, 1 ; skip blanking cursor
	call NewLine
	jp .end
.noWidthOverflow:
	ldh [ConsoleOnColumn], a
.end:
	ret

PollGetCharFlagValue:
	; a: getChar flag value
	di
	ld a, [CGetCharFlag]
	ei
	ret;

getc:
	; wait for char
	; returns
	; a: char
	di
	ld a, 1
	ld [CGetCharFlag], a
	ei
.loop:
	halt
	call PollGetCharFlagValue
	cp a, 1
	jp z, .loop
	ld a, [rSB]
	ret

putc:
	; b: character to output
	push de
	push hl
	push bc
		ldh a, [ConsoleOnRow]
		ld d, a
		ldh a, [ConsoleOnColumn]
		ld e, a
		; check for newline
		ld a, $0a
		cp a, b
		jp z, .newLine
		; check for backspace
		ld a, BackspaceAsciiVal
		cp a, b
		jp z, .backspace
		;call WriteCharDE
		call GetVRAMAddressInHL
		ld e, l
		ld d, h
		call EnqueueVRAMChange
		jp .noNewLine
.newLine:
		ld a, 0 ; don't skip blanking cursor
		call NewLine
		jp .end
.backspace:
		; if we're at the start of the line, then no backspace
		ldh a, [ConsoleOnColumn]
		cp a, 0
		jp z, .end
		push af
		call EnqueueBlankCurrentCursor
		pop af
		dec a
		ld [ConsoleOnColumn], a
		ld e, a
		ld a, [ConsoleOnRow]
		ld d, a
		call GetVRAMAddressInHL
		ld e, l
		ld d, h
		ld b, $20
		call EnqueueVRAMChange
		jp .end
.noNewLine:
		call AdvanceCursor
.end:
		ld a, TerminalCursorKey
		ld [CCursorHoverValue], a
	pop bc
	pop hl
	pop de
	ret 

FPrint:
	; de: num letters
	; hl: string ptr
	di
	jp .test
.loopStart:
	ld a, [hli]
	ld b, a
	call putc
	dec de
.test:
	ld a, e
	cp a, 0
	jp z, .isZero
	
	jp .loopStart
.isZero:
	ld a, d
	cp a, 0
	jp z, .endLoop
	jp .loopStart
.endLoop:
	ei
	ret
; CONSOLE FUNCTIONS END

TestString:
db "Hello World!"

CompareBytes:
	; a length
	; hl bytes 1
	; de bytes 2
	; returns:
	; a: are bytes the same 1: yes, 0: no
	push de
	push hl
	push bc
		ld c, a
.loop:
		ld a, [hli]
		ld b, a
		push hl
			ld h, d
			ld l, e
			ld a, [hli]
			ld d, h
			ld e, l
		pop hl
		cp a, b
		jp z, .equal
		jp .loopEndFailure
.equal:
	dec c
	ld a, 0
	cp a, c
	jp nz, .loop
.loopEndSuccess:
	ld a, 1
	jp .end
.loopEndFailure:
	ld a, 0
.end:
	pop bc
	pop hl
	pop de
	ret
Word2Find: db "bro"
EntryPoint:
	ld sp, $fffe
	call Init
	; Now we turn on the LCD display to view the results!
	;ld hl, TestString
	;ld de, 12
	;call FPrint
	;StoreCellLiteralHRAM IPtr, Thread
	
	;jp Top
	; serial - set as slave
	ld a, 1
	ld [rSB], a
    ld a, $80
	ld [rSC], a
	
	;ld de, Word2Find
	;ld b, 3
	;call FindWord 
MainLoop:
	;halt
	call getc
	ld b, a
	call putc
	jp MainLoop

FindWord:
	; de: word chars ptr
	; b: word length
	; returns:
	; hl: word address
	; b: was found

	ld hl, DictionaryStart
.loop:
	ld a, [hl]
	cp a, b
	jp z, .nameLengthEqual
	jp .nameLengthNotEqual
.nameLengthEqual:
	push bc
	push hl
		ld c, WordNameOffset
		ld b, 0
		add hl, bc
		call CompareBytes
	pop hl
	pop bc
	cp a, 1
	jp z, .found
	jp .notFound
.found:
	ld b, 1
	jp .loopEnd
.notFound:
.nameLengthNotEqual:
	push bc
	ld b, 0
	ld c, WordNextOffset
	add hl,  bc
	ld a, [hli]
	ld c, a
	ld a, [hli]
	ld b, a

	ld h, b
	ld l, c

	ld a, b
	cp a, 0
	jp z, .highZero
	pop bc
	jp .loop
.highZero:
	ld a, c
	pop bc
	cp a, 0
	jp nz, .loop
	ld b, 0
.loopEnd:
	ret


Thread:
	dw PushData.body
	dw 400
	dw PushData.body
	dw -20
	dw AddData.body

	dw PushData.body
	dw 420
	dw PushData.body
	dw 130
	dw SubData.body

	dw PushData.body
	dw 3
	dw PushData.body
	dw 3
	dw MulData.body

	dw PushData.body
	dw 40
	dw PushData.body
	dw 10
	dw MulData.body

	;dw PushData.body
	;dw 12
	;dw PushData.body
	;dw 3
	;dw DivData.body

	dw MainLoop

DictionaryStart:
	WordHeader PopData, PushData, pop, 3
		MPopData c, b
		jp Top

	WordHeader PushData, AddData, push, 4
		ReadCellLiteral c, b
		MPushData c, b
		jp Top

	WordHeader AddData, SubData, +, 1
		MPopData c, b
		MPopData e, d
		ld a, c
		add a, e
		ld e, a ; cache a in e
		ld a, b
		adc a, d
		ld d, a ; cache a in d
		MPushData e, d
		jp Top

	WordHeader SubData, StoreCell, -, 1
		MPopData c, b
		MPopData e, d
		ld a, e
		sub a, c
		ld c, a
		ld a, d
		sbc a, b
		ld b, a
		MPushData c, b
		jp Top

	WordHeader StoreCell, FetchCell, !, 1
		; ( val address -- )
		MPopData l, h 
		MPopData c, b
		ld a, c
		ld [hli], a
		ld a, b
		ld [hl], a
		jp Top

	WordHeader FetchCell, StoreByte, @, 1
		; ( address -- CellValueAt )
		MPopData l, h
		ld [hli], a
		ld c, a
		ld [hl], a
		ld b, a
		MPushData c, b
		jp Top

	WordHeader StoreByte, FetchByte, c!, 2
		MPopData l, h
		MPopData c, b
		ld [hl], c
		jp Top

	WordHeader FetchByte, RelativeBranch, c@, 2
		; ( address -- ByteValueAt )
		MPopData l, h
		ld a, [hl]
		ld c, a
		ld b, 0
		MPushData c, b
		jp Top

	WordHeader RelativeBranch, RelativeBranchIfZero, br, 2
		ReadCellLiteral c, b
		LoadCellHRAM l, h, IPtr
		add hl, bc
		StoreCellFromRegisterPairHRAM l, h, IPtr
		jp Top

	WordHeader RelativeBranchIfZero, Return, br0, 3
		MPopData l, h
		ld a, 0
		cp a, h
		jp nz, .notZero
		cp a, l
		jp nz, .notZero
		PeekCellLiteral c, b           ; done like this to be like my C forth version - only skip over the branch offset literal if we don't branch
		LoadCellHRAM l, h, IPtr
		add hl, bc
		StoreCellFromRegisterPairHRAM l, h, IPtr
		jp Top
	.notZero:
		ReadCellLiteral c, b
		jp Top

	WordHeader Return, MulData, ret, 3
		MPopReturn l, h
		StoreCellFromRegisterPairHRAM l, h, IPtr
		jp Top

	WordHeader MulData, DivData, *, 1
		MPopData c, b 
		MPopData e, d
		MMultiply
		ld b, h
		ld c, l
		MPushData c, b
		jp Top

	WordHeader DivData, Emit, /, 1
		MPopData c, b 
		MPopData e, d
		MDivide
		MPushData e, d
		jp Top

	WordHeader Emit, 0, emit, 4
		MPopData c, b
		ld b, c
		call putc
		jp Top

