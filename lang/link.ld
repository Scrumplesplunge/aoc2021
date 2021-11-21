/* Place all program output in a single program segment */
PHDRS {
  all PT_LOAD FILEHDR PHDRS;
}

/* The program entry point */
ENTRY(_start)

/* Lay out sections with minimal alignment overhead, starting at 0x10000 */
SECTIONS {
  . = 0x10000 + SIZEOF_HEADERS;
  .text ALIGN(0x1) : { *(.text*) } :all
  .rodata ALIGN(0x4) : { *(.rodata*) }
  .data ALIGN(0x4) : { *(.data*) }
  .bss ALIGN(0x4) : { *(.bss*) }
  /DISCARD/ : {
    *(*)
  }
}
