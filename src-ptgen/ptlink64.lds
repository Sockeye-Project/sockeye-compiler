OUTPUT_FORMAT("elf64-x86-64")
OUTPUT_ARCH("i386:x86-64")

PHDRS
{
        load PT_LOAD;
        noload PT_NULL;
}

SECTIONS {
        . = 0;
        __pagetables_start = . ;
        .pagetables 0x0 : ALIGN(4k)
        {
            __pagetables_L1_start = . ;
           *(.pagetables.l0);
           __pagetables_L2_start = . ;
           *(.pagetables.l1);
           __pagetables_L3_start = . ;
           *(.pagetables.l2);
           __pagetables_L4_start = . ;
           *(.pagetables.l3);
        } : load
        __pagetables_end = .;
        . = ALIGN(4k);

         __devmappings_start = . ;
         .devmappings . :
         {
            *(.devmappings.maps);
            __efimmap_start = . ;
            *(.devmappings.efimmap);
            __efimmap_end = . ;
         }
         __devmappings_end = . ;

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
