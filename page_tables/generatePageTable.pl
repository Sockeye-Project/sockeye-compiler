%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(decodingNet).
:- use_module(decodingNetQueries).
:- use_module(pageTableGenerator).

generate(Net,Template) :-
    split_string(Net,""," ",[NetPath]),
    split_string(Template,""," ",[TemplatePath]),
	loadnet(NetPath),
	CPU = 'CORTEXA9',
	DeviceIds = ['UART3','GIC_Intr_Distributor','CKGEN_PRM','CKGEN_CM1'],
	MemoryIds = ['SDRAM'],
	(
        param(CPU),
        foreach(Id,DeviceIds),
        fromto([],Prev,Next,DeviceRegions)
    do
        findDeviceRegion(CPU,Id,(Range,_)),
        Next = [Range|Prev]
    ),
	(
        param(CPU),
        foreach(Id,MemoryIds),
        fromto([],Prev,Next,MemoryRegions)
    do
        findMemoryRegion(CPU,Id,(Range,_)),
        Next = [Range|Prev]
    ),
	pageTable(DeviceRegions,MemoryRegions,Table),
    templateReplace(TemplatePath,Table,HeaderFile),
    write(HeaderFile).

templateReplace(TemplatePath,PageTable,Result) :-
    open(TemplatePath,read,TemplateFile),
    read_string(TemplateFile,end_of_file,_,Template),
    close(TemplateFile),
    split_string(Template,"?","",[Before,_,After]),
    concat_string([Before,PageTable,After],Result).
