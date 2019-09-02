bits 16	   ; 16-bit mode

org 7C00h ; address of the boot sector

BootStageOne:
	cli
	mov ax, CSEG
	mov es, ax
	mov ds, ax
	sti

	mov ah, 00h	; BIOS - reset disk
	mov dl, 0	; set drive number
	int 13h		; call BIOS disk services

	mov ah, 02h	; read sectors
	mov al, 10h	; set number of sectors to read (16)
	mov dl, 0	; set drive number
	mov ch, 0	; set cylinder number
	mov dh, 0	; set head number
	mov cl, 2	; set starting sector number
	mov bx, CSEG	; set memory location to load to
	int 13h		; call BIOS disk services

	jmp CSEG:start	; "far" jump to program

CSEG equ 7E00h

EOS0:
	; pad remaining space in sector
	times ((200h - 2) - ($ - $$)) db 0
	; write boot signature
	dw 0AA55h

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VRAM_320x200 equ 0a000h
VRAM_80x20 equ 0b800h
VIDEO_HEIGHT equ 320
VIDEO_WIDTH equ 200
VIDEO_SIZE equ VIDEO_HEIGHT * VIDEO_WIDTH
cursor_y: dw 1
cursor_x: dw 0
box_size: dw 30h	; not too big, not too small
box_color: dw 7		; default to gray
box_y_coord: dw (200/2)-(30h/2)
box_x_coord: dw (320/2)-(30h/2)
box_xy_step: dw 4
box_size_step: dw 4
screen_color: dw 1	; default to blue
welcome: db '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz`~!@#$%^&*()-_=+{[]}|\;:,<.>?/',0

start:
	push word 13h		; 320x200 VGA, 256 colors
	call set_graphics_mode

	.mainloop:
		; reset cursor position
		mov word [cursor_x], 0
		mov word [cursor_y], 0

		push word [screen_color]
		call clear_screen

		push welcome
		call puts

		push word [box_size]	 ; length
		push word [box_color]	 ; color
		push word [box_x_coord]	 ; x
		push word [box_y_coord]	 ; y
		call draw_box

		call input_handler

		push word 0800h
		push word 0000h
		call sleep		; prevent color-change induced seizures

		jmp .mainloop

	cli	; disable interrupts
	hlt	; halt CPU interrupts


puts:
	push bp
	mov bp, sp
	push ax
	push si

	mov si, [bp + 4]
	xor ax, ax

	.write:
		lodsb
		cmp ax, 0
		je .return
		call putc
		jmp .write

	.return:
	pop si
	pop ax
	pop bp
	ret 2 * 1

putc:
	call draw_char
	add word [cursor_x], 8
	cmp word [cursor_x], 320
	jl .return
	mov word [cursor_x], 0
	add word [cursor_y], 8
	.return:
		ret

draw_char:
	push bp
	push ax
	push bx
	push cx
	push si

	mov si, font
	;sub ax, 'A'
	sub ax, ' '
	shl ax, 3
	add si, ax

	push word [cursor_x]
	push word [cursor_y]
	push word 8
	push si
	call draw_font

	pop si
	pop cx
	pop bx
	pop ax
	pop bp
	ret

input_handler:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx

	mov bx, [box_xy_step]
	mov cx, [box_size_step]

	mov ax, 0000h
	int 16h

	cmp al, ' '
	je .screen_color_next

	cmp al, 'x'
	je .screen_color_next

	cmp al, 'z'
	je .screen_color_prev

	cmp al, ']'
	je .box_color_next

	cmp al, '['
	je .box_color_prev

	cmp al, '+'
	je .box_size_bigger

	cmp al, '-'
	je .box_size_smaller

	cmp ah, 75		; scan-code: left arrow
	je .box_move_left

	cmp ah, 77		; scan-code: right arrow
	je .box_move_right

	cmp ah, 72		; scan-code: up arrow
	je .box_move_up

	cmp ah, 80		; scan-code: down arrow
	je .box_move_down

	jmp .return

	.screen_color_next:
		inc byte [screen_color]
		jmp .return

	.screen_color_prev:
		dec byte [screen_color]
		jmp .return

	.box_color_next:
		inc byte [box_color]
		jmp .return

	.box_color_prev:
		dec byte [box_color]
		jmp .return

	.box_size_bigger:
		add [box_size], cx
		jmp .return

	.box_size_smaller:
		sub [box_size], cx
		jmp .return

	.box_move_up:
		sub word [box_y_coord], bx
		jmp .return

	.box_move_down:
		add word [box_y_coord], bx
		jmp .return

	.box_move_left:
		sub word [box_x_coord], bx
		jmp .return

	.box_move_right:
		add word [box_x_coord], bx
		jmp .return

	.return:
		pop cx
		pop bx
		pop ax
		pop bp
		ret


sleep:
	push bp
	mov bp, sp
	push ax
	push cx
	push dx
	mov ah, 86h
	mov cx, [bp + 4]
	mov dx, [bp + 6]
	int 15h
	pop dx
	pop cx
	pop ax
	mov sp, bp
	pop bp
	ret


set_graphics_mode:
	push bp
	mov bp, sp

	push ax
	mov ah, 0
	mov al, [bp + 4]
	int 10h
	pop ax
	pop bp
	ret 2 * 1


clear_screen:
	push bp
	mov bp, sp

	push ax
	push cx
	push di
	push es
	mov ax, VRAM_320x200
	mov es, ax

	mov ax, [bp + 4]
	mov ah, al
	xor di, di
	mov cx, VIDEO_SIZE / 2
	.loop:
		stosw
		dec cx
		jne .loop

	pop es
	pop di
	pop cx
	pop ax
	mov sp, bp
	pop bp
	ret 2 * 1


draw_pixel:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx
	push di

	push es
	mov ax, VRAM_320x200
	mov es, ax

	mov ax, [bp + 4]	; Y coord (row)
	mov bx, [bp + 6]	; X coord (col)
	mov cx, VIDEO_HEIGHT
	mul cx
	add ax, bx
	cmp ax, VIDEO_SIZE
	ja .return

	mov di, ax
	mov dx, [bp + 8]	; color

	mov byte [es:di], dl
	.return:
		pop es
		pop di
		pop dx
		pop cx
		pop bx
		pop ax

		pop bp
		ret 2 * 3


draw_horizontal:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx

	mov ax, [bp + 4]	; Y coord (row)
	mov bx, [bp + 6]	; X coord (col)
	mov dx, [bp + 8]	; color

	mov cx, [bp + 10]   ; length
	.draw:
		push dx	 ; color
		push bx	 ; X coord (col)
		push ax	 ; Y coord (row)
		call draw_pixel
		inc bx
		dec cx
		jne .draw

	pop dx
	pop cx
	pop bx
	pop ax

	mov sp, bp
	pop bp
	ret 2 * 4


draw_vertical:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx

	mov ax, [bp + 4]	; Y coord (row)
	mov bx, [bp + 6]	; X coord (col)
	mov dx, [bp + 8]	; color

	mov cx, [bp + 10]   ; length
	.draw:
		push dx
		push bx
		push ax
		call draw_pixel
		inc ax
		dec cx
		jne .draw

	pop dx
	pop cx
	pop bx
	pop ax

	mov sp, bp
	pop bp
	ret 2 * 4


draw_box:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx

	mov ax, [bp + 4]	; Y coord (at row)
	mov bx, [bp + 6]	; X coord (at col)
	mov dx, [bp + 8]	; color

	mov cx, [bp + 10]   ; length

	.draw_top:
		push cx
		push dx
		push bx
		push ax
		call draw_horizontal
		dec cx
		ja .draw_top

	mov cx, [bp + 10]
	.draw_left:
		push cx
		push dx
		push bx
		push ax
		call draw_vertical
		dec cx
		ja .draw_left

	mov cx, [bp + 10]
	push ax
	add ax, cx
	.draw_bottom:
		push cx
		push dx
		push bx
		push ax
		call draw_horizontal
		dec cx
		ja .draw_bottom

	mov cx, [bp + 10]
	pop ax
	add bx, cx
	dec bx
	.draw_right:
		push cx
		push dx
		push bx
		push ax
		call draw_vertical
		dec cx
		ja .draw_right

	pop dx
	pop cx
	pop bx
	pop ax

	mov sp, bp
	pop bp
	ret 2 * 4


draw_triangle:
	push bp
	mov bp, sp

	nop

	pop bp
	ret

draw_font:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov si, [bp + 4]	; bitmap address
	mov bx, [bp + 6]	; length of bitmap*8
	mov di, [bp + 8]	; Y coord
	mov dx, [bp + 10]	; X coord

	mov cx, 8				; 8 byte font (8x8 pixels)
	.loadbmp:
		lodsb				; load byte es:si
		.write:
			test al, 1		; check current bit in byte 0=skip, 1=print
			jz .skip_bit
			push word 7		; color=gray (for now)
			push dx			; x coord
			push di			; y coord
			call draw_pixel
			.skip_bit:
				shr al, 1	; next bit in byte
				inc di		; next Y coord
				dec bx		; decrement total length counter
				jne .write
		inc dx				; next X coord
		dec cx				; decrement byte counter
		jne .loadbmp

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax

	pop bp
	ret 2 * 4


charset_dump:
	push word bx
	push word 0
	push word 8
	push si
	call draw_font
	add si, 8
	add bx, 8
	dec cx
	jne charset_dump
	ret

font:
	; SPACE ' '
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	; !
	db 00000000b
	db 00000000b
	db 00000000b
	db 01101111b
	db 01101111b
	db 00000000b
	db 00000000b
	db 00000000b
	; "
	db 00000000b
	db 00000111b
	db 00001111b
	db 00000000b
	db 00000000b
	db 00001111b
	db 00000111b
	db 00000000b
	; #
	db 00000000b
	db 00100010b
	db 01111111b
	db 00100010b
	db 00100010b
	db 01111111b
	db 00100010b
	db 00000000b
	; $
	db 00000000b
	db 00100100b
	db 00101010b
	db 00101010b
	db 01111111b
	db 00101010b
	db 00110010b
	db 00000000b
	; %
	db 00000000b
	db 00100011b
	db 00010011b
	db 00001000b
	db 00000100b
	db 00110010b
	db 00110001b
	db 00000000b
	; &
	db 00000000b
	db 00101110b
	db 01010101b
	db 00101011b
	db 01010000b
	db 00000000b
	db 00000000b
	db 00000000b
	; '
	db 00000000b
	db 00001111b
	db 00000111b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	; (
	db 00000000b
	db 00000000b
	db 00000000b
	db 00111110b
	db 01000001b
	db 00000000b
	db 00000000b
	db 00000000b
	; )
	db 00000000b
	db 00000000b
	db 00000000b
	db 01000001b
	db 00111110b
	db 00000000b
	db 00000000b
	db 00000000b
	; *
	db 00000000b
	db 00100010b
	db 00010100b
	db 01111111b
	db 00010100b
	db 00100010b
	db 00000000b
	db 00000000b
	; +
	db 00000000b
	db 00000000b
	db 00001000b
	db 00001000b
	db 01111111b
	db 00001000b
	db 00001000b
	db 00000000b
	; ,
	db 00000000b
	db 00000000b
	db 00000000b
	db 01110000b
	db 11110000b
	db 00000000b
	db 00000000b
	db 00000000b
	; -
	db 00000000b
	db 00001000b
	db 00001000b
	db 00001000b
	db 00001000b
	db 00001000b
	db 00001000b
	db 00000000b
	; .
	db 00000000b
	db 01100000b
	db 01100000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	; /
	db 00000000b
	db 00100000b
	db 00010000b
	db 00001000b
	db 00000100b
	db 00000010b
	db 00000001b
	db 00000000b
	; 0
	db 00000000b
	db 00011100b
	db 00110010b
	db 01001001b
	db 01000101b
	db 00100010b
	db 00011100b
	db 00000000b
	; 1
	db 00000000b
	db 01000000b
	db 01000010b
	db 01000001b
	db 01111111b
	db 01000000b
	db 01000000b
	db 00000000b
	; 2
	db 00000000b
	db 01000010b
	db 01100001b
	db 01010001b
	db 01001001b
	db 01000101b
	db 01000010b
	db 00000000b
	; 3
	db 00000000b
	db 00100010b
	db 01000001b
	db 01000001b
	db 01001001b
	db 01001001b
	db 00111110b
	db 00000000b
	; 4
	db 00000000b
	db 00001000b
	db 00001100b
	db 00001010b
	db 01111111b
	db 00001000b
	db 00000000b
	db 00000000b
	; 5
	db 00000000b
	db 01001111b
	db 01001001b
	db 01001001b
	db 01001001b
	db 00111001b
	db 00000000b
	db 00000000b
	; 6
	db 00000000b
	db 00111110b
	db 01001001b
	db 01001001b
	db 01001001b
	db 01001001b
	db 01111001b
	db 00000000b
	; 7
	db 00000000b
	db 01000001b
	db 00101001b
	db 00011001b
	db 00001001b
	db 00001101b
	db 00001011b
	db 00000000b
	; 8
	db 00000000b
	db 01110111b
	db 01001001b
	db 01001001b
	db 01001001b
	db 01001001b
	db 01110111b
	db 00000000b
	; 9
	db 00000000b
	db 00001111b
	db 00001001b
	db 00001001b
	db 00001001b
	db 00001001b
	db 01111111b
	db 00000000b
	; :
	db 00000000b
	db 00000000b
	db 00000000b
	db 00110011b
	db 00110011b
	db 00000000b
	db 00000000b
	db 00000000b
	; ;
	db 00000000b
	db 00000000b
	db 00000000b
	db 01110011b
	db 00110011b
	db 00000000b
	db 00000000b
	db 00000000b
	; <
	db 00000000b
	db 00001000b
	db 00010100b
	db 00100010b
	db 01000001b
	db 00000000b
	db 00000000b
	db 00000000b
	; =
	db 00000000b
	db 00100100b
	db 00100100b
	db 00100100b
	db 00100100b
	db 00100100b
	db 00100100b
	db 00000000b
	; >
	db 00000000b
	db 00000000b
	db 00000000b
	db 01000001b
	db 00100010b
	db 00010100b
	db 00001000b
	db 00000000b
	; ?
	db 00000000b
	db 00000010b
	db 00000001b
	db 01011001b
	db 00001001b
	db 00001001b
	db 00000110b
	db 00000000b
	; @
	db 00000000b
	db 00111110b
	db 01000001b
	db 01011001b
	db 01111001b
	db 01000001b
	db 00111110b
	db 00000000b
	; A
	db 00000000b
	db 01111100b
	db 00010010b
	db 00010001b
	db 00010001b
	db 00010010b
	db 01111100b
	db 00000000b
	; B
	db 00000000b
	db 01111111b
	db 01001001b
	db 01001001b
	db 01001001b
	db 01001001b
	db 01110110b
	db 00000000b
	; C
	db 00000000b
	db 00111110b
	db 01000001b
	db 01000001b
	db 01000001b
	db 01000001b
	db 01000001b
	db 00000000b
	; D
	db 00000000b
	db 01111111b
	db 01000001b
	db 01000001b
	db 01000001b
	db 01000001b
	db 00111110b
	db 00000000b
	; E
	db 00000000b
	db 01111111b
	db 01001001b
	db 01001001b
	db 01001001b
	db 01001001b
	db 01001001b
	db 00000000b
	; F
	db 00000000b
	db 01111111b
	db 00001001b
	db 00001001b
	db 00001001b
	db 00000001b
	db 00000001b
	db 00000000b
	; G
	db 00000000b
	db 01111110b
	db 01000001b
	db 01000001b
	db 01001001b
	db 01001001b
	db 01111001b
	db 00000000b
	; H
	db 00000000b
	db 01111111b
	db 00001000b
	db 00001000b
	db 00001000b
	db 00001000b
	db 01111111b
	db 00000000b
	; I
	db 00000000b
	db 01000001b
	db 01000001b
	db 01111111b
	db 01111111b
	db 01000001b
	db 01000001b
	db 00000000b
	; J
	db 00000000b
	db 00110000b
	db 01000000b
	db 01000001b
	db 01000001b
	db 00111111b
	db 00000001b
	db 00000000b
	; K
	db 00000000b
	db 01111111b
	db 00001000b
	db 00010100b
	db 00100010b
	db 01000001b
	db 00000000b
	db 00000000b
	; L
	db 00000000b
	db 01111111b
	db 01000000b
	db 01000000b
	db 01000000b
	db 01000000b
	db 01000000b
	db 00000000b
	; M
	db 00000000b
	db 01111111b
	db 00000010b
	db 00000100b
	db 00000100b
	db 00000010b
	db 01111111b
	db 00000000b
	; N
	db 00000000b
	db 01111111b
	db 00000010b
	db 00000100b
	db 00001000b
	db 00010000b
	db 01111111b
	db 00000000b
	; O
	db 00000000b
	db 00111110b
	db 01000001b
	db 01000001b
	db 01000001b
	db 01000001b
	db 00111110b
	db 00000000b
	; P
	db 00000000b
	db 01111111b
	db 00001001b
	db 00001001b
	db 00001001b
	db 00001001b
	db 00000110b
	db 00000000b
	; Q
	db 00000000b
	db 00111110b
	db 01000001b
	db 01000001b
	db 01010001b
	db 01100001b
	db 01111110b
	db 10000000b ; << COULD IT BE THE LINE OF THE 'Q'? REALLY?
	; R
	db 00000000b
	db 01111111b
	db 00001001b
	db 00001001b
	db 00011001b
	db 00101001b
	db 01000110b
	db 00000000b
	; S
	db 00000000b
	db 01100110b
	db 01001001b
	db 01001001b
	db 01001001b
	db 01001001b
	db 00110011b
	db 00000000b
	db 00000000b ; << WHY THE EXTRA BYTE? WHERE THE FUCK IS THE PROBLEM!?
	; T
	db 00000000b
	db 00000001b
	db 00000001b
	db 01111111b
	db 01111111b
	db 00000001b
	db 00000001b
	db 00000000b
	; U
	db 00000000b
	db 00111111b
	db 01000000b
	db 01000000b
	db 01000000b
	db 01000000b
	db 00111111b
	db 00000000b
	; V
	db 00000000b
	db 00011111b
	db 00100000b
	db 01000000b
	db 01000000b
	db 00100000b
	db 00011111b
	db 00000000b
	; W
	db 00000000b
	db 00111111b
	db 01000000b
	db 00100000b
	db 00111000b
	db 01000000b
	db 00111111b
	db 00000000b
	; X
	db 00000000b
	db 01100011b
	db 00010100b
	db 00001000b
	db 00001000b
	db 00010100b
	db 01100011b
	db 00000000b
	; Y
	db 00000000b
	db 00000011b
	db 00000100b
	db 01111000b
	db 01111000b
	db 00000100b
	db 00000011b
	db 00000000b
	; Z
	db 00000000b
	db 01100001b
	db 01010001b
	db 01001001b
	db 01000101b
	db 01000011b
	db 01000001b
	db 00000000b
	; [
	db 00000000b
	db 00000000b
	db 00000000b
	db 01111111b
	db 01000001b
	db 00000000b
	db 00000000b
	db 00000000b
	; \
	db 00000000b
	db 00000001b
	db 00000010b
	db 00000100b
	db 00001000b
	db 00010000b
	db 00100000b
	db 00000000b
	; ]
	db 00000000b
	db 00000000b
	db 00000000b
	db 01000001b
	db 01111111b
	db 00000000b
	db 00000000b
	db 00000000b
	; ^
	db 00000000b
	db 00000100b
	db 00000010b
	db 00000001b
	db 00000010b
	db 00000100b
	db 00000000b
	db 00000000b
	; _
	db 00000000b
	db 01000000b
	db 01000000b
	db 01000000b
	db 01000000b
	db 01000000b
	db 01000000b
	db 00000000b
	; `
	db 00000000b
	db 00000000b
	db 00000001b
	db 00000010b
	db 00000100b
	db 00000000b
	db 00000000b
	db 00000000b
	; a
	db 00000000b
	db 00000000b
	db 00110000b
	db 01001010b
	db 01001010b
	db 01111100b
	db 00000000b
	db 00000000b
	; b
	db 00000000b
	db 00000000b
	db 01111111b
	db 01001000b
	db 01001000b
	db 01110000b
	db 00000000b
	db 00000000b
	; c
	db 00000000b
	db 00000000b
	db 00111000b
	db 01000100b
	db 01000100b
	db 01000100b
	db 00000000b
	db 00000000b
	; d
	db 00000000b
	db 00000000b
	db 00110000b
	db 01001000b
	db 01001000b
	db 01001000b
	db 00111111b
	db 00000000b
	; e
	db 00000000b
	db 00000000b
	db 00111000b
	db 01010100b
	db 01010100b
	db 01010100b
	db 01011000b
	db 00000000b
	; f
	db 00000000b
	db 00000000b
	db 01001000b
	db 01111110b
	db 00001001b
	db 00000001b
	db 00000010b
	db 00000000b
	; g
	db 00000000b
	db 00000000b
	db 00011000b
	db 10100100b
	db 10100100b
	db 10100100b
	db 01111110b
	db 00000000b
	; h
	db 00000000b
	db 00000000b
	db 01111111b
	db 00001000b
	db 00001000b
	db 01110000b
	db 00000000b
	db 00000000b
	; i
	db 00000000b
	db 00000000b
	db 00000000b
	db 01111010b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	; j
	db 00000000b
	db 00000000b
	db 01100000b
	db 10000000b
	db 10001000b
	db 01111010b
	db 00000000b
	db 00000000b
	; k
	db 00000000b
	db 00000000b
	db 01111110b
	db 00010000b
	db 00101000b
	db 01000000b
	db 00000000b
	db 00000000b
	; l
	db 00000000b
	db 00000000b
	db 00000010b
	db 01111110b
	db 10000000b
	db 00000000b
	db 00000000b
	db 00000000b
	; m
	db 00000000b
	db 00000000b
	db 01111000b
	db 00000100b
	db 00111000b
	db 00000100b
	db 01111000b
	db 00000000b
	; n
	db 00000000b
	db 00000000b
	db 01111100b
	db 00001000b
	db 00000100b
	db 00000100b
	db 01111000b
	db 00000000b
	; o
	db 00000000b
	db 00000000b
	db 00111000b
	db 01000100b
	db 01000100b
	db 01000100b
	db 00111000b
	db 00000000b
	; p
	db 00000000b
	db 00000000b
	db 11111110b
	db 00100100b
	db 00100100b
	db 00100100b
	db 00011000b
	db 00000000b
	; q
	db 00000000b
	db 00000000b
	db 00011000b
	db 00100100b
	db 00100100b
	db 00100100b
	db 11111110b
	db 00000000b
	; r
	db 00000000b
	db 00000000b
	db 01111100b
	db 00001000b
	db 00000100b
	db 00000100b
	db 00001000b
	db 00000000b
	; s
	db 00000000b
	db 00000000b
	db 01001000b
	db 01010100b
	db 01010100b
	db 00100100b
	db 00000000b
	db 00000000b
	; t
	db 00000000b
	db 00000000b
	db 00000100b
	db 00111110b
	db 01000100b
	db 00000000b
	db 00000000b
	db 00000000b
	; u
	db 00000000b
	db 00000000b
	db 00111100b
	db 01000000b
	db 01000000b
	db 01111100b
	db 00000000b
	db 00000000b
	; v
	db 00000000b
	db 00000000b
	db 00111100b
	db 01000000b
	db 00111100b
	db 00000000b
	db 00000000b
	db 00000000b
	; w
	db 00000000b
	db 00000000b
	db 01111100b
	db 00100000b
	db 00110000b
	db 00100000b
	db 01111100b
	db 00000000b
	; x
	db 00000000b
	db 00000000b
	db 01000000b
	db 00100100b
	db 00011000b
	db 00100100b
	db 01000000b
	db 00000000b
	; y
	db 00000000b
	db 00000000b
	db 00000000b
	db 00011100b
	db 10100000b
	db 01100000b
	db 00011100b
	db 00000000b
	; z
	db 00000000b
	db 00000000b
	db 01000100b
	db 01100100b
	db 01010100b
	db 01001100b
	db 01000100b
	db 00000000b
	; {
	db 00000000b
	db 00000000b
	db 00001000b
	db 00110110b
	db 01000001b
	db 01000001b
	db 00000000b
	db 00000000b
	; |
	db 00000000b
	db 00000000b
	db 00000000b
	db 11111111b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	; }
	db 00000000b
	db 00000000b
	db 01000001b
	db 01000001b
	db 00110110b
	db 00001000b
	db 00000000b
	db 00000000b
	; ~
	db 00000000b
	db 00000000b
	db 00010000b
	db 00001000b
	db 00010000b
	db 00001000b
	db 00000000b
	db 00000000b
	; DEL
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b



	; SMILEY
	;db 00000000b
	;db 01111110b
	;db 10010101b
	;db 10100001b
	;db 10100001b
	;db 10010101b
	;db 01111110b
	;db 00000000b
