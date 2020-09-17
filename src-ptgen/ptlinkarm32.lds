OUTPUT_FORMAT("elf32-littlearm")
OUTPUT_ARCH("arm")

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
        .pagetables 0x1000 : ALIGN(4k)
        {
            page_table_root = . ;
            __pagetables_L1_start = . ;
           *(.pagetables.L1);
           __pagetables_L2_start = . ;
           *(.pagetables.L2);
        } : load
        __pagetables_end = .;
        . = ALIGN(4k);

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
