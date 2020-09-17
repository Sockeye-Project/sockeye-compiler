%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(page_table_generator).

%%%%%%%%%%%
%% Utils %%
%%%%%%%%%%%


str_node(Str,Node) :-
    split_string(Str, ".", "", NodeR),
    reverse(NodeR, Node),
    ( node_id_node_enum(Node, _)
      ; (write("!! Node not found: "),writeln(Node),fail)
    ).

pprint(map(VAddr, Size, PAddr, Label, Prop)) :-
    EndAddr is VAddr+Size-1,
    node_str(Label,LabelStr),
    printf(" [0x%16rULL..0x%16rULL] -> 0x%16rULL     %p,%p\n",
        [VAddr, EndAddr, PAddr,LabelStr, Prop]).
pprint(region(Id, [block(Base,Limit)], Prop)) :-
    printf(" region(%p, [block(0x%16r,0x%16r)], %p)\n",
        [Id, Base, Limit, Prop]).
pprint((A,B)) :-
    write("<"),
    pprint(A),
    write(", "),
    pprint(B),
    write(">\n\n").

read_template(TemplatePath,Content) :-
    open(TemplatePath,read,TemplateFile),
    read_string(TemplateFile,end_of_file,_,Content),
    close(TemplateFile).

write_file(OutPath,Content) :-
    open(OutPath,write,OutFile),
    write(OutFile,Content),
    close(OutFile).

%%%%%%%%%%%%%%%%
%% Page Table %%
%%%%%%%%%%%%%%%%

pt_map(InNode, map(InBase, Size, OutAddr, DestNode, Prop)) :-
    InR=region(InNode,[block(InBase,InLimit)],_),
    decodes_to_accept(InR, AccR, _),
    decode_step(InR, InR, OutR),
    Size is InLimit - InBase + 1,
    OutR = region(_,[block(OutAddr,_)],_),
    AccR = region(DestNode,_,Prop).

% InNode should be the Physical node
pt_mappings(InNode, Mappings) :-
    findall(X,pt_map(InNode, X), Mappings).


gen_pt(ModuleStr, InNodeStr, TemplateFile, OutFile, PTBase, TargetArg) :-
    printf("=== Generating Pagetable %p ===\n", [OutFile]),
    decoding_net_load_module(ModuleStr),
    node_str(InNode, InNodeStr),
    %assert_node_exists(InNode),
    translate(region(InNode,_,_), region(OutNode,_,_)),
    printf("Generating PT between nodes: %p -> %p\n", [InNode, OutNode]),
    pt_mappings(InNode, Maps),
    printf("Using the following Mappings: \n", []),
    checklist(pprint, Maps),
    printf("\n", []),
    split_string(TemplateFile,""," ",[TemplatePath]),
    read_template(TemplatePath,Template),
    generate_pt_code(Template, Maps, PTBase, TargetArg, CodeNoEfi),
    !, printf("Code generation successful! Out = %s\n", [OutFile]),
    !,
    efimap(InNode, EfiStr),
    sprintf(Code, CodeNoEfi, [EfiStr]),
    !,
    split_string(OutFile,""," ",[OutPath]),
    write_file(OutPath,Code).

%%%%%%%%%%%%%%
%% BootInfo %%
%%%%%%%%%%%%%%

kernel_region_i(region(Id, [block(0,_)], true)) :-
    (node_find_fuzzy("DRAM", Id) ; node_find_fuzzy("DRAM_0", Id)).
kernel_region(R) :-
    kernel_region_i(Ri), !, R = Ri.

gic_dist(R) :-
    node_find_fuzzy("distributor", Id),
    R = region(Id, _, _),
    accept(R).

gic_redist(R) :-
    node_find_fuzzy("redistributor", Id),
    R = region(Id, _, _),
    accept(R).


uart(R) :-
    node_find_fuzzy("pl011_uart", Id),
    R = region(Id, _, _),
    accept(R).

boot_uart(Node, R) :-
    % Pick uart that is visible from Node
    uart(R).
    %decodes_rev(R, _, region(Node, _, _)).

gen_bootinfo(ModuleStr, BootVNodeStr, OutFile) :-
    printf("=== Generating Bootinfo %p Module=%s, ===\n", [OutFile, ModuleStr]),
    decoding_net_load_module(ModuleStr),
    str_node(BootVNodeStr, BootVNode),
    translate(region(BootVNode,_,_), region(BootPNode,_,_)),
    printf("Boot Virtual Node = %p\n", [BootVNode]),
    printf("Boot Physical Node = %p\n", [BootPNode]),
    (boot_uart(BootVNode, UARTRegion);throw("Boot UART Node not found")),
    printf("Boot UART = ", []), pprint(UARTRegion),
    kernel_region(KRegion),
    KRegion = region(_,[block(KBase,KLimit)], _),
    KLimit is KBase + (1024*1024), % TODO FIGURE OUT SIZE
    printf("Boot Kernel Load Region = ", []), pprint(KRegion),
    
    UARTVRegion = region(BootVNode, [block(UARTVBase,_)], _),
    decodes_rev(UARTRegion, _, UARTVRegion),
    printf("UARTVBase = %p\n", UARTVBase),
    UARTPRegion = region(BootPNode, [block(UARTPBase,_)], _),
    decodes_rev(UARTRegion, _, UARTPRegion),

    KIn = region(BootVNode, [block(ImageDest,_)], _),
    decodes_rev(KRegion, _, KIn),

    % the boot driver executes at address 0 -> TODO: pass as arguments ?
    BootdriverBase = 0, 

    %CPUDriverWindow is ImageDest + 16'ffff000000000000, %'

    % we put the page-tables at the first byte of the kernel
    % KernelPageTables = ImageDest, 

    BSPMpidr = 0, 

    Tmpl = "#include <boot_consts.h>

struct bootinfo_consts bootinfo_consts = {
    // reads 0x30013001 // patched by the image creation
    .magic  = BOOT_COREDATA_MAGIC,               
    // multi-processor identifyer to start execution
    .mpidr  = 0x%16r,               
    
    // bootdriver local virtual address of the image base
    .image_base  = 0x%16r,          
    
    // size of the image in bytes
    .image_size  = 0x0,          
    
    // bootdriver local virtual address of the image destination
    .image_dest  = 0x%16r,      
    
    .coredata_ptr = 0x0,

    .uart_pbase = 0x%16r,
    .uart_vbase = 0x%16r
};",

    sprintf(Code, Tmpl, [BSPMpidr, BootdriverBase, ImageDest, UARTPBase, UARTVBase]),

    split_string(OutFile,""," ",[OutPath]),
    write_file(OutPath,Code).

%%%%%%%%%%%%%
%% EFI Map %%
%%%%%%%%%%%%%

% Mapped Regions for the efi map
efi_mapped(Node, (In,Out)) :-
    In = region(Node, _, _),
    decodes_to_accept(In, Out, _).

% Unmapped Regions for the efi map
efi_unmapped(Node, Um) :-
    findall(Out, efi_mapped(Node, (_,Out)), Mapped),
    accept(UmS),
    reg_subtract_li(UmS, Mapped, Um).

prop_efi_attribute(OutNode, mem, "EFI_MEMORY_BF_LOAD") :-  kernel_region(region(OutNode, _, _)).
prop_efi_attribute(_, mem, "0").
prop_efi_attribute(_, devreg, "EFI_MEMORY_UC").
prop_efi_attribute(_, bootrom, "EFI_MEMORY_RO").
prop_efi_attribute(A,B,C) :- 
    sprintf(Err, "ERROR prop_efi_attribute(%p,%p,%p) UNDEFINED\n", [A,B,C]),
    throw(Err).
region_efi_type(region(_,_,devreg), "EfiMemoryMappedIO").
region_efi_type(region(_,_,mem), "EfiConventionalMemory").
region_efi_type(region(_,_,bootrom), "EfiLoaderCode").
region_efi_type(A,B) :- 
    sprintf(Err, "ERROR region_efi_type(%p,%p) UNDEFINED\n", [A,B]),
    throw(Err).

vaddr_to_str(invalid, "-1").
vaddr_to_str(V, Str) :- not(V = invalid), sprintf(Str, "0x%16r", V).

efi_entry_to_c((In,Out), Str) :-
    % TODO: Determine pagesize dynamically
    PageSize is 4*1024,
    In = region(_, [block(VirtualStart, _)], _),
    Out = region(OutNode, [block(PhysicalStart, PhysicalEnd)], Prop),
    node_enum(OutNode,Pad),
    node_str(OutNode, OutNodeStr),
    region_efi_type(Out, Type),
    prop_efi_attribute(OutNode, Prop, Attribute),
    NumberOfPages is (PhysicalEnd - PhysicalStart + 1) // PageSize,
    vaddr_to_str(VirtualStart, VirtualStartStr),
    sprintf(Str, "    EFI_MMAP_ENTRY(%s, %d /* %s */, 0x%16r, %s, 0x%16r, %s), ",
        [Type,Pad, OutNodeStr, PhysicalStart,VirtualStartStr,NumberOfPages,Attribute]).

efi_entry_to_c(Out, Str) :-
    efi_entry_to_c( (region(_,[block(invalid,_)],_), Out), Str).

efimap(Node, Str) :-
    setof((In,Out), efi_mapped(Node, (In,Out)), Mapped),
    setof(Um, efi_unmapped(Node, Um), UnMapped),
    maplist(efi_entry_to_c, Mapped, MappedStrs),
    maplist(efi_entry_to_c, UnMapped, UnMappedStrs),
    append(MappedStrs,UnMappedStrs, AllStrs),
    join_string(AllStrs, "\n", StrT),
    sprintf(Str, "%s", StrT).

gen_efimap(ModuleStr, NodeStr, OutFile) :-
    decoding_net_load_module(ModuleStr),
    str_node(NodeStr, Node),
    efimap(Node, Str),
    write_file(OutFile, Str),
    printf("Wrote EFI map to %p\n", [OutFile]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CPU driver translate functions %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c_range_trans(InBase,Size,OutBase, C) :-
    InLimit is InBase + Size - 1,
    ( OutBase >= InBase -> 
        sprintf(C, "    if(0x%16rUL <= arg && arg <= 0x%16rUL) return ((0x%16rUL - 0x%16rUL) + arg);",
        [InBase, InLimit, OutBase, InBase])
        ;
        sprintf(C, "    if(0x%16rUL <= arg && arg <= 0x%16rUL) return (0x%16rUL + (arg - 0x%16rUL));",
        [InBase, InLimit, OutBase, InBase]) 
    ).

c_range_check(InBase,Size, C) :-
    InLimit is InBase + Size - 1,
    sprintf(C, "    if(0x%16rUL <= arg && arg <= 0x%16rUL) return 1;",
        [InBase, InLimit]).

% mem to local phys
c_mtp_e(map(InBase, Size, OutAddr, _, _), C) :-
    c_range_trans(InBase, Size, OutAddr, C).

% local phys to mem
c_ptm_e(map(InBase, Size, OutAddr, _, _), C) :-
    c_range_trans(OutAddr, Size, InBase, C).

% local phys valid
c_p_valid_e(map(_, Size, OutAddr, _, _), C) :-
    c_range_check(OutAddr, Size, C).

c_gtp_valid_e(map(_, Size, OutAddr, _, _), C) :-
    c_range_check(OutAddr, Size, C).

c_mtp(Maps, C) :-
    maplist(c_mtp_e, Maps, MapsStrs),
    join_string(MapsStrs, "\n", C).
    % sprintf(C, "lpaddr_t mem_to_local_phys(lvaddr_t arg){\n%s\n    assert(false);\n};", [MapsStr]).

c_ptm(Maps, C) :-
    maplist(c_ptm_e, Maps, MapsStrs),
    join_string(MapsStrs, "\n", C).
    % sprintf(C, "lvaddr_t local_phys_to_mem(lpaddr_t arg){\n%s\n    assert(false);\n};", [MapsStr]).

c_p_valid(Maps, C) :-
    maplist(c_p_valid_e, Maps, MapsStrs),
    join_string(MapsStrs, "\n", C).
    % printf(C, "bool local_phys_is_valid(lpaddr_t addr){\n%s\n    return 0;\n};", [MapsStr]).

c_ptg_e(InNode, C) :-
    InR=region(InNode,block(InBase,InLimit),_),
    decodes_to_accept(InR, AccR, _),
    AccR = region(DestNode,block(OutBase,_),_),
    node_id_node_enum(DestNode, DestNodeEnum), % Must use non-choice pred here
    ( OutBase >= InBase -> 
        sprintf(C, "    if(0x%16rUL <= arg && arg <= 0x%16rUL) return gname_implode(%d, ((0x%16rUL - 0x%16rUL) + arg));",
        [InBase, InLimit, DestNodeEnum, OutBase, InBase])
        ;
        sprintf(C, "    if(0x%16rUL <= arg && arg <= 0x%16rUL) return gname_implode(%d, (0x%16rUL + (arg - 0x%16rUL)));",
        [InBase, InLimit, DestNodeEnum, OutBase, InBase]) 
    ).
c_ptg(InNode, C) :-
    findall(C, c_ptg_e(InNode, C), Cs),
    join_string(Cs, "\n", C).

% lpaddr_t global_name_to_local_phys(gname_t arg)
c_gtp_e(InNode, C) :-
    InR=region(InNode,block(InBase,_),_),
    decodes_to_accept(InR, AccR, _),
    AccR = region(DestNode,block(OutBase,OutLimit),_),
    node_id_node_enum(DestNode, DestNodeEnum), % Must use non-choice pred here
    sprintf(CIdMatch, "asid == %d", [DestNodeEnum]),

    ( InBase >= OutBase -> 
        sprintf(C, "    if(%s && 0x%16rUL <= ad && ad <= 0x%16rUL) return ((0x%16rUL - 0x%16rUL) + ad);",
        [CIdMatch, OutBase, OutLimit, InBase, OutBase])
        ;
        sprintf(C, "    if(%s && 0x%16rUL <= ad && ad <= 0x%16rUL) return (0x%16rUL + (ad - 0x%16rUL));",
        [CIdMatch, OutBase, OutLimit, InBase, OutBase]) 
    ).

c_gtp(InNode, C) :-
    findall(C, c_gtp_e(InNode, C), MapsStrs),
    Preamble1 = "    addr_t ad = gname_extract_addr(arg);",
    Preamble2 = "    asid_t asid = gname_extract_asid(arg);",
    join_string([Preamble1, Preamble2 | MapsStrs], "\n", C).


node_offset(Node, Offset) :-
    translate(region(Node,block(Start,_),_), name(_,End,_)),
    Offset is Start - End.

c_assert_offset(Node, C) :-
    node_offset(Node, Offset),
    printf("Node=%p, Offset=%p\n", [Node,Offset]),
    sprintf(C, "STATIC_ASSERT(0x%16r == ARMV8_KERNEL_OFFSET, \"Kernel Offset\");", [Offset]).

% Base is expressed in PNode
uart_in_node(PNode, BaseS, SizeS) :-
    PReg = region(PNode, block(Base,_), _),
    uart(DReg),
    DReg = region(_, block(DBase, DLimit), _),
    Size is DLimit - DBase + 1,
    decodes_rev(DReg, _, PReg),
    sprintf(BaseS,"0x%16r", Base),
    sprintf(SizeS,"0x%16r", Size).

c_uarts(PNode, Bases, Sizes, Num) :-
    findall(B, uart_in_node(PNode, B, _), BasesArr),
    findall(S, uart_in_node(PNode, _, S), SizesArr),
    join_string(BasesArr, ",", Bases),
    join_string(SizesArr, ",", Sizes),
    length(BasesArr, Num).

c_gic(PNode, DistBaseS, RedistBaseS) :-
    gic_dist(DistDest),
    DistSrc = region(PNode, block(DistBase,_), _),
    decodes_rev(DistDest, _, DistSrc),
    sprintf(DistBaseS, "0x%16r", DistBase),

    gic_redist(RedistDest),
    RedistSrc = region(PNode, block(RedistBase,_), _),
    decodes_rev(RedistDest, _, RedistSrc),
    sprintf(RedistBaseS, "0x%16r", RedistBase).


gen_cpudriver_trans(ModuleStr, InNodeStr, InTempl, OutFile) :-
    printf("=== Generating platform file %p ===\n", [OutFile]),
    decoding_net_load_module(ModuleStr),
    split_string(InTempl,""," ",[TemplatePath]),
    split_string(OutFile,""," ",[OutFilePath]),
    read_template(TemplatePath,Template),
    str_node(InNodeStr, InNode),
    pt_mappings(InNode, Maps),
    %c_assert_offset(InNode, OffsetAssert),
    c_mtp(Maps, LVAtoLPA),
    c_ptm(Maps, LPAtoLVA),
    c_p_valid(Maps, LPAisValid),

    translate(region(InNode,_,_), name(PVNode,_,_)),
    c_gtp(PVNode, GNtoLPA),
    c_ptg(PVNode, LPAtoGN),

    (c_gic(PVNode, GIC_DIST_BASE, GIC_REDIST_BASE) ; throw("GIC Nodes not found")),

    (c_uarts(PVNode, UART_BASES, UART_SIZES, MAX_NUM_UARTS) ; throw("No UART nodes found")),

    SERIAL_NUM_PORTS = 1,
    SERIAL_CONSOLE_PORT = 0,
    SERIAL_DEBUG_PORT = 0,

    join_string(["\"",ModuleStr, "\""], "", PLATFORM_STRING),


    sprintf(Code, Template,
        [
        % platform string
        PLATFORM_STRING,
        % GIC
        GIC_DIST_BASE, GIC_REDIST_BASE,
        % serial
        MAX_NUM_UARTS, SERIAL_CONSOLE_PORT, SERIAL_DEBUG_PORT,
        SERIAL_NUM_PORTS, 
        UART_BASES, UART_SIZES,
        % Functions
        LPAisValid, LPAtoLVA, LVAtoLPA, GNtoLPA, LPAtoGN]),

    write_file(OutFilePath, Code),

    % TODO: genpaddr <-> lpaddr
    printf("Wrote C translation functions to %p\n", [OutFilePath]).




