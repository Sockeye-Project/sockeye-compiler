OUTPUT_FORMAT("elf32-i386")
OUTPUT_ARCH("i386")

PHDRS
{
        load PT_LOAD;
        noload PT_NULL;
}

SECTIONS {
        . = 0;
        .devmappings . :
        {
           *(.devmappings.maps);
        }
        __pagetables_start = . ;
        .pagetables 0x4000 : ALIGN(16k)
        {
            page_table_root = . ;
            __pagetables_L1_start = . ;
           *(.pagetables.L1);
           __pagetables_L2_start = . ;
           *(.pagetables.L2);
           __pagetables_L3_start = . ;
           *(.pagetables.L3);
           __pagetables_L4_start = . ;
           *(.pagetables.L4);
        } : load
        __pagetables_end = .;
        . = ALIGN(16k);

        .text . :
        {
           *(.text);
        }

        .dynsym . :
        {
           *(.dynsym);
        }

        /DISCARD/ :
        {
                *(.comment);
                *(.eh_frame);
                *(.eh_frame_hdr);
                *(.note.gnu.build-id);
                *(.interp);
                *(.dynamic);
        }
}
