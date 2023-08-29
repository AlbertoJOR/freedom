module linear_layer_0 (
    input [63:0] x0, x1, x2, x3, x4,
    input [63:0] r0, r1, r2, r3, r4,
    output [63:0] Y0, Y1, Y2, Y3, Y4
);


    // Matrix multiplication algorithm

        wire [319:0] s, r;
        assign s = {x0,x1,x2,x3,x4};
        assign r = {r0,r1,r2,r3,r4};
        
        assign Y0 = {s[319] ^ s[283] ^ s[274] ^ r[319], s[318] ^ s[282] ^ s[273] ^ r[318], s[317] ^ s[281] ^ s[272] ^ r[317], s[316] ^ s[280] ^ s[271] ^ r[316], s[315] ^ s[279] ^ s[270] ^ r[315], s[314] ^ s[278] ^ s[269] ^ r[314], s[313] ^ s[277] ^ s[268] ^ r[313], s[312] ^ s[276] ^ s[267] ^ r[312],
                    s[311] ^ s[275] ^ s[266] ^ r[311], s[310] ^ s[274] ^ s[265] ^ r[310], s[309] ^ s[273] ^ s[264] ^ r[309], s[308] ^ s[272] ^ s[263] ^ r[308], s[307] ^ s[271] ^ s[262] ^ r[307], s[306] ^ s[270] ^ s[261] ^ r[306], s[305] ^ s[269] ^ s[260] ^ r[305], s[304] ^ s[268] ^ s[259] ^ r[304],
                    s[303] ^ s[267] ^ s[258] ^ r[303], s[302] ^ s[266] ^ s[257] ^ r[302], s[301] ^ s[265] ^ s[256] ^ r[301], s[319] ^ s[300] ^ s[264] ^ r[319], s[318] ^ s[299] ^ s[263] ^ r[318], s[317] ^ s[298] ^ s[262] ^ r[317], s[316] ^ s[297] ^ s[261] ^ r[316], s[315] ^ s[296] ^ s[260] ^ r[315],
                    s[314] ^ s[295] ^ s[259] ^ r[314], s[313] ^ s[294] ^ s[258] ^ r[313], s[312] ^ s[293] ^ s[257] ^ r[312], s[311] ^ s[292] ^ s[256] ^ r[311], s[319] ^ s[310] ^ s[291] ^ r[319], s[318] ^ s[309] ^ s[290] ^ r[318], s[317] ^ s[308] ^ s[289] ^ r[317], s[316] ^ s[307] ^ s[288] ^ r[316],
                    s[315] ^ s[306] ^ s[287] ^ r[315], s[314] ^ s[305] ^ s[286] ^ r[314], s[313] ^ s[304] ^ s[285] ^ r[313], s[312] ^ s[303] ^ s[284] ^ r[312], s[311] ^ s[302] ^ s[283] ^ r[311], s[310] ^ s[301] ^ s[282] ^ r[310], s[309] ^ s[300] ^ s[281] ^ r[309], s[308] ^ s[299] ^ s[280] ^ r[308],
                    s[307] ^ s[298] ^ s[279] ^ r[307], s[306] ^ s[297] ^ s[278] ^ r[306], s[305] ^ s[296] ^ s[277] ^ r[305], s[304] ^ s[295] ^ s[276] ^ r[304], s[303] ^ s[294] ^ s[275] ^ r[303], s[302] ^ s[293] ^ s[274] ^ r[302], s[301] ^ s[292] ^ s[273] ^ r[301], s[300] ^ s[291] ^ s[272] ^ r[300],
                    s[299] ^ s[290] ^ s[271] ^ r[299], s[298] ^ s[289] ^ s[270] ^ r[298], s[297] ^ s[288] ^ s[269] ^ r[297], s[296] ^ s[287] ^ s[268] ^ r[296], s[295] ^ s[286] ^ s[267] ^ r[295], s[294] ^ s[285] ^ s[266] ^ r[294], s[293] ^ s[284] ^ s[265] ^ r[293], s[292] ^ s[283] ^ s[264] ^ r[292],
                    s[291] ^ s[282] ^ s[263] ^ r[291], s[290] ^ s[281] ^ s[262] ^ r[290], s[289] ^ s[280] ^ s[261] ^ r[289], s[288] ^ s[279] ^ s[260] ^ r[288], s[287] ^ s[278] ^ s[259] ^ r[287], s[286] ^ s[277] ^ s[258] ^ r[286], s[285] ^ s[276] ^ s[257] ^ r[285], s[284] ^ s[275] ^ s[256] ^ r[284]};

        assign Y1 = {s[255] ^ s[252] ^ s[230] ^ r[255], s[254] ^ s[251] ^ s[229] ^ r[254], s[253] ^ s[250] ^ s[228] ^ r[253], s[252] ^ s[249] ^ s[227] ^ r[252], s[251] ^ s[248] ^ s[226] ^ r[251], s[250] ^ s[247] ^ s[225] ^ r[250], s[249] ^ s[246] ^ s[224] ^ r[249], s[248] ^ s[245] ^ s[223] ^ r[248],
                    s[247] ^ s[244] ^ s[222] ^ r[247], s[246] ^ s[243] ^ s[221] ^ r[246], s[245] ^ s[242] ^ s[220] ^ r[245], s[244] ^ s[241] ^ s[219] ^ r[244], s[243] ^ s[240] ^ s[218] ^ r[243], s[242] ^ s[239] ^ s[217] ^ r[242], s[241] ^ s[238] ^ s[216] ^ r[241], s[240] ^ s[237] ^ s[215] ^ r[240],
                    s[239] ^ s[236] ^ s[214] ^ r[239], s[238] ^ s[235] ^ s[213] ^ r[238], s[237] ^ s[234] ^ s[212] ^ r[237], s[236] ^ s[233] ^ s[211] ^ r[236], s[235] ^ s[232] ^ s[210] ^ r[235], s[234] ^ s[231] ^ s[209] ^ r[234], s[233] ^ s[230] ^ s[208] ^ r[233], s[232] ^ s[229] ^ s[207] ^ r[232],
                    s[231] ^ s[228] ^ s[206] ^ r[231], s[230] ^ s[227] ^ s[205] ^ r[230], s[229] ^ s[226] ^ s[204] ^ r[229], s[228] ^ s[225] ^ s[203] ^ r[228], s[227] ^ s[224] ^ s[202] ^ r[227], s[226] ^ s[223] ^ s[201] ^ r[226], s[225] ^ s[222] ^ s[200] ^ r[225], s[224] ^ s[221] ^ s[199] ^ r[224],
                    s[223] ^ s[220] ^ s[198] ^ r[223], s[222] ^ s[219] ^ s[197] ^ r[222], s[221] ^ s[218] ^ s[196] ^ r[221], s[220] ^ s[217] ^ s[195] ^ r[220], s[219] ^ s[216] ^ s[194] ^ r[219], s[218] ^ s[215] ^ s[193] ^ r[218], s[217] ^ s[214] ^ s[192] ^ r[217], s[255] ^ s[216] ^ s[213] ^ r[255],
                    s[254] ^ s[215] ^ s[212] ^ r[254], s[253] ^ s[214] ^ s[211] ^ r[253], s[252] ^ s[213] ^ s[210] ^ r[252], s[251] ^ s[212] ^ s[209] ^ r[251], s[250] ^ s[211] ^ s[208] ^ r[250], s[249] ^ s[210] ^ s[207] ^ r[249], s[248] ^ s[209] ^ s[206] ^ r[248], s[247] ^ s[208] ^ s[205] ^ r[247],
                    s[246] ^ s[207] ^ s[204] ^ r[246], s[245] ^ s[206] ^ s[203] ^ r[245], s[244] ^ s[205] ^ s[202] ^ r[244], s[243] ^ s[204] ^ s[201] ^ r[243], s[242] ^ s[203] ^ s[200] ^ r[242], s[241] ^ s[202] ^ s[199] ^ r[241], s[240] ^ s[201] ^ s[198] ^ r[240], s[239] ^ s[200] ^ s[197] ^ r[239],
                    s[238] ^ s[199] ^ s[196] ^ r[238], s[237] ^ s[198] ^ s[195] ^ r[237], s[236] ^ s[197] ^ s[194] ^ r[236], s[235] ^ s[196] ^ s[193] ^ r[235], s[234] ^ s[195] ^ s[192] ^ r[234], s[255] ^ s[233] ^ s[194] ^ r[255], s[254] ^ s[232] ^ s[193] ^ r[254], s[253] ^ s[231] ^ s[192] ^ r[253]};

        assign Y2 = {s[191] ^ s[133] ^ s[128] ^ r[191], s[191] ^ s[190] ^ s[132] ^ r[191], s[190] ^ s[189] ^ s[131] ^ r[190], s[189] ^ s[188] ^ s[130] ^ r[189], s[188] ^ s[187] ^ s[129] ^ r[188], s[187] ^ s[186] ^ s[128] ^ r[187], s[191] ^ s[186] ^ s[185] ^ r[191], s[190] ^ s[185] ^ s[184] ^ r[190],
                    s[189] ^ s[184] ^ s[183] ^ r[189], s[188] ^ s[183] ^ s[182] ^ r[188], s[187] ^ s[182] ^ s[181] ^ r[187], s[186] ^ s[181] ^ s[180] ^ r[186], s[185] ^ s[180] ^ s[179] ^ r[185], s[184] ^ s[179] ^ s[178] ^ r[184], s[183] ^ s[178] ^ s[177] ^ r[183], s[182] ^ s[177] ^ s[176] ^ r[182],
                    s[181] ^ s[176] ^ s[175] ^ r[181], s[180] ^ s[175] ^ s[174] ^ r[180], s[179] ^ s[174] ^ s[173] ^ r[179], s[178] ^ s[173] ^ s[172] ^ r[178], s[177] ^ s[172] ^ s[171] ^ r[177], s[176] ^ s[171] ^ s[170] ^ r[176], s[175] ^ s[170] ^ s[169] ^ r[175], s[174] ^ s[169] ^ s[168] ^ r[174],
                    s[173] ^ s[168] ^ s[167] ^ r[173], s[172] ^ s[167] ^ s[166] ^ r[172], s[171] ^ s[166] ^ s[165] ^ r[171], s[170] ^ s[165] ^ s[164] ^ r[170], s[169] ^ s[164] ^ s[163] ^ r[169], s[168] ^ s[163] ^ s[162] ^ r[168], s[167] ^ s[162] ^ s[161] ^ r[167], s[166] ^ s[161] ^ s[160] ^ r[166],
                    s[165] ^ s[160] ^ s[159] ^ r[165], s[164] ^ s[159] ^ s[158] ^ r[164], s[163] ^ s[158] ^ s[157] ^ r[163], s[162] ^ s[157] ^ s[156] ^ r[162], s[161] ^ s[156] ^ s[155] ^ r[161], s[160] ^ s[155] ^ s[154] ^ r[160], s[159] ^ s[154] ^ s[153] ^ r[159], s[158] ^ s[153] ^ s[152] ^ r[158],
                    s[157] ^ s[152] ^ s[151] ^ r[157], s[156] ^ s[151] ^ s[150] ^ r[156], s[155] ^ s[150] ^ s[149] ^ r[155], s[154] ^ s[149] ^ s[148] ^ r[154], s[153] ^ s[148] ^ s[147] ^ r[153], s[152] ^ s[147] ^ s[146] ^ r[152], s[151] ^ s[146] ^ s[145] ^ r[151], s[150] ^ s[145] ^ s[144] ^ r[150],
                    s[149] ^ s[144] ^ s[143] ^ r[149], s[148] ^ s[143] ^ s[142] ^ r[148], s[147] ^ s[142] ^ s[141] ^ r[147], s[146] ^ s[141] ^ s[140] ^ r[146], s[145] ^ s[140] ^ s[139] ^ r[145], s[144] ^ s[139] ^ s[138] ^ r[144], s[143] ^ s[138] ^ s[137] ^ r[143], s[142] ^ s[137] ^ s[136] ^ r[142],
                    s[141] ^ s[136] ^ s[135] ^ r[141], s[140] ^ s[135] ^ s[134] ^ r[140], s[139] ^ s[134] ^ s[133] ^ r[139], s[138] ^ s[133] ^ s[132] ^ r[138], s[137] ^ s[132] ^ s[131] ^ r[137], s[136] ^ s[131] ^ s[130] ^ r[136], s[135] ^ s[130] ^ s[129] ^ r[135], s[134] ^ s[129] ^ s[128] ^ r[134]};

        assign Y3 = {s[127] ^ s[80] ^ s[73] ^ r[127], s[126] ^ s[79] ^ s[72] ^ r[126], s[125] ^ s[78] ^ s[71] ^ r[125], s[124] ^ s[77] ^ s[70] ^ r[124], s[123] ^ s[76] ^ s[69] ^ r[123], s[122] ^ s[75] ^ s[68] ^ r[122], s[121] ^ s[74] ^ s[67] ^ r[121], s[120] ^ s[73] ^ s[66] ^ r[120],
                    s[119] ^ s[72] ^ s[65] ^ r[119], s[118] ^ s[71] ^ s[64] ^ r[118], s[127] ^ s[117] ^ s[70] ^ r[127], s[126] ^ s[116] ^ s[69] ^ r[126], s[125] ^ s[115] ^ s[68] ^ r[125], s[124] ^ s[114] ^ s[67] ^ r[124], s[123] ^ s[113] ^ s[66] ^ r[123], s[122] ^ s[112] ^ s[65] ^ r[122],
                    s[121] ^ s[111] ^ s[64] ^ r[121], s[127] ^ s[120] ^ s[110] ^ r[127], s[126] ^ s[119] ^ s[109] ^ r[126], s[125] ^ s[118] ^ s[108] ^ r[125], s[124] ^ s[117] ^ s[107] ^ r[124], s[123] ^ s[116] ^ s[106] ^ r[123], s[122] ^ s[115] ^ s[105] ^ r[122], s[121] ^ s[114] ^ s[104] ^ r[121],
                    s[120] ^ s[113] ^ s[103] ^ r[120], s[119] ^ s[112] ^ s[102] ^ r[119], s[118] ^ s[111] ^ s[101] ^ r[118], s[117] ^ s[110] ^ s[100] ^ r[117], s[116] ^ s[109] ^ s[99] ^ r[116], s[115] ^ s[108] ^ s[98] ^ r[115], s[114] ^ s[107] ^ s[97] ^ r[114], s[113] ^ s[106] ^ s[96] ^ r[113],
                    s[112] ^ s[105] ^ s[95] ^ r[112], s[111] ^ s[104] ^ s[94] ^ r[111], s[110] ^ s[103] ^ s[93] ^ r[110], s[109] ^ s[102] ^ s[92] ^ r[109], s[108] ^ s[101] ^ s[91] ^ r[108], s[107] ^ s[100] ^ s[90] ^ r[107], s[106] ^ s[99] ^ s[89] ^ r[106], s[105] ^ s[98] ^ s[88] ^ r[105],
                    s[104] ^ s[97] ^ s[87] ^ r[104], s[103] ^ s[96] ^ s[86] ^ r[103], s[102] ^ s[95] ^ s[85] ^ r[102], s[101] ^ s[94] ^ s[84] ^ r[101], s[100] ^ s[93] ^ s[83] ^ r[100], s[99] ^ s[92] ^ s[82] ^ r[99], s[98] ^ s[91] ^ s[81] ^ r[98], s[97] ^ s[90] ^ s[80] ^ r[97],
                    s[96] ^ s[89] ^ s[79] ^ r[96], s[95] ^ s[88] ^ s[78] ^ r[95], s[94] ^ s[87] ^ s[77] ^ r[94], s[93] ^ s[86] ^ s[76] ^ r[93], s[92] ^ s[85] ^ s[75] ^ r[92], s[91] ^ s[84] ^ s[74] ^ r[91], s[90] ^ s[83] ^ s[73] ^ r[90], s[89] ^ s[82] ^ s[72] ^ r[89],
                    s[88] ^ s[81] ^ s[71] ^ r[88], s[87] ^ s[80] ^ s[70] ^ r[87], s[86] ^ s[79] ^ s[69] ^ r[86], s[85] ^ s[78] ^ s[68] ^ r[85], s[84] ^ s[77] ^ s[67] ^ r[84], s[83] ^ s[76] ^ s[66] ^ r[83], s[82] ^ s[75] ^ s[65] ^ r[82], s[81] ^ s[74] ^ s[64] ^ r[81]};

        assign Y4 = {s[63] ^ s[40] ^ s[6] ^ r[63], s[62] ^ s[39] ^ s[5] ^ r[62], s[61] ^ s[38] ^ s[4] ^ r[61], s[60] ^ s[37] ^ s[3] ^ r[60], s[59] ^ s[36] ^ s[2] ^ r[59], s[58] ^ s[35] ^ s[1] ^ r[58], s[57] ^ s[34] ^ s[0] ^ r[57], s[63] ^ s[56] ^ s[33] ^ r[63],
                    s[62] ^ s[55] ^ s[32] ^ r[62], s[61] ^ s[54] ^ s[31] ^ r[61], s[60] ^ s[53] ^ s[30] ^ r[60], s[59] ^ s[52] ^ s[29] ^ r[59], s[58] ^ s[51] ^ s[28] ^ r[58], s[57] ^ s[50] ^ s[27] ^ r[57], s[56] ^ s[49] ^ s[26] ^ r[56], s[55] ^ s[48] ^ s[25] ^ r[55],
                    s[54] ^ s[47] ^ s[24] ^ r[54], s[53] ^ s[46] ^ s[23] ^ r[53], s[52] ^ s[45] ^ s[22] ^ r[52], s[51] ^ s[44] ^ s[21] ^ r[51], s[50] ^ s[43] ^ s[20] ^ r[50], s[49] ^ s[42] ^ s[19] ^ r[49], s[48] ^ s[41] ^ s[18] ^ r[48], s[47] ^ s[40] ^ s[17] ^ r[47],
                    s[46] ^ s[39] ^ s[16] ^ r[46], s[45] ^ s[38] ^ s[15] ^ r[45], s[44] ^ s[37] ^ s[14] ^ r[44], s[43] ^ s[36] ^ s[13] ^ r[43], s[42] ^ s[35] ^ s[12] ^ r[42], s[41] ^ s[34] ^ s[11] ^ r[41], s[40] ^ s[33] ^ s[10] ^ r[40], s[39] ^ s[32] ^ s[9] ^ r[39],
                    s[38] ^ s[31] ^ s[8] ^ r[38], s[37] ^ s[30] ^ s[7] ^ r[37], s[36] ^ s[29] ^ s[6] ^ r[36], s[35] ^ s[28] ^ s[5] ^ r[35], s[34] ^ s[27] ^ s[4] ^ r[34], s[33] ^ s[26] ^ s[3] ^ r[33], s[32] ^ s[25] ^ s[2] ^ r[32], s[31] ^ s[24] ^ s[1] ^ r[31],
                    s[30] ^ s[23] ^ s[0] ^ r[30], s[63] ^ s[29] ^ s[22] ^ r[63], s[62] ^ s[28] ^ s[21] ^ r[62], s[61] ^ s[27] ^ s[20] ^ r[61], s[60] ^ s[26] ^ s[19] ^ r[60], s[59] ^ s[25] ^ s[18] ^ r[59], s[58] ^ s[24] ^ s[17] ^ r[58], s[57] ^ s[23] ^ s[16] ^ r[57],
                    s[56] ^ s[22] ^ s[15] ^ r[56], s[55] ^ s[21] ^ s[14] ^ r[55], s[54] ^ s[20] ^ s[13] ^ r[54], s[53] ^ s[19] ^ s[12] ^ r[53], s[52] ^ s[18] ^ s[11] ^ r[52], s[51] ^ s[17] ^ s[10] ^ r[51], s[50] ^ s[16] ^ s[9] ^ r[50], s[49] ^ s[15] ^ s[8] ^ r[49],
                    s[48] ^ s[14] ^ s[7] ^ r[48], s[47] ^ s[13] ^ s[6] ^ r[47], s[46] ^ s[12] ^ s[5] ^ r[46], s[45] ^ s[11] ^ s[4] ^ r[45], s[44] ^ s[10] ^ s[3] ^ r[44], s[43] ^ s[9] ^ s[2] ^ r[43], s[42] ^ s[8] ^ s[1] ^ r[42], s[41] ^ s[7] ^ s[0] ^ r[41]};

endmodule

module linear_layer_1 (
    input [63:0] x0, x1, x2, x3, x4,
    input [63:0] r0, r1, r2, r3, r4,
    output [63:0] Y0, Y1, Y2, Y3, Y4
);


    // Matrix multiplication algorithm

        wire [319:0] s,r;
        assign s = {x0,x1,x2,x3,x4};
        assign r = {r0,r1,r2,r3,r4};

        assign Y0 = {s[319] ^ s[283] ^ s[274] ^ r[283], s[318] ^ s[282] ^ s[273] ^ r[282], s[317] ^ s[281] ^ s[272] ^ r[281], s[316] ^ s[280] ^ s[271] ^ r[280], s[315] ^ s[279] ^ s[270] ^ r[279], s[314] ^ s[278] ^ s[269] ^ r[278], s[313] ^ s[277] ^ s[268] ^ r[277], s[312] ^ s[276] ^ s[267] ^ r[276],
                    s[311] ^ s[275] ^ s[266] ^ r[275], s[310] ^ s[274] ^ s[265] ^ r[274], s[309] ^ s[273] ^ s[264] ^ r[273], s[308] ^ s[272] ^ s[263] ^ r[272], s[307] ^ s[271] ^ s[262] ^ r[271], s[306] ^ s[270] ^ s[261] ^ r[270], s[305] ^ s[269] ^ s[260] ^ r[269], s[304] ^ s[268] ^ s[259] ^ r[268],
                    s[303] ^ s[267] ^ s[258] ^ r[267], s[302] ^ s[266] ^ s[257] ^ r[266], s[301] ^ s[265] ^ s[256] ^ r[265], s[319] ^ s[300] ^ s[264] ^ r[300], s[318] ^ s[299] ^ s[263] ^ r[299], s[317] ^ s[298] ^ s[262] ^ r[298], s[316] ^ s[297] ^ s[261] ^ r[297], s[315] ^ s[296] ^ s[260] ^ r[296],
                    s[314] ^ s[295] ^ s[259] ^ r[295], s[313] ^ s[294] ^ s[258] ^ r[294], s[312] ^ s[293] ^ s[257] ^ r[293], s[311] ^ s[292] ^ s[256] ^ r[292], s[319] ^ s[310] ^ s[291] ^ r[310], s[318] ^ s[309] ^ s[290] ^ r[309], s[317] ^ s[308] ^ s[289] ^ r[308], s[316] ^ s[307] ^ s[288] ^ r[307],
                    s[315] ^ s[306] ^ s[287] ^ r[306], s[314] ^ s[305] ^ s[286] ^ r[305], s[313] ^ s[304] ^ s[285] ^ r[304], s[312] ^ s[303] ^ s[284] ^ r[303], s[311] ^ s[302] ^ s[283] ^ r[302], s[310] ^ s[301] ^ s[282] ^ r[301], s[309] ^ s[300] ^ s[281] ^ r[300], s[308] ^ s[299] ^ s[280] ^ r[299],
                    s[307] ^ s[298] ^ s[279] ^ r[298], s[306] ^ s[297] ^ s[278] ^ r[297], s[305] ^ s[296] ^ s[277] ^ r[296], s[304] ^ s[295] ^ s[276] ^ r[295], s[303] ^ s[294] ^ s[275] ^ r[294], s[302] ^ s[293] ^ s[274] ^ r[293], s[301] ^ s[292] ^ s[273] ^ r[292], s[300] ^ s[291] ^ s[272] ^ r[291],
                    s[299] ^ s[290] ^ s[271] ^ r[290], s[298] ^ s[289] ^ s[270] ^ r[289], s[297] ^ s[288] ^ s[269] ^ r[288], s[296] ^ s[287] ^ s[268] ^ r[287], s[295] ^ s[286] ^ s[267] ^ r[286], s[294] ^ s[285] ^ s[266] ^ r[285], s[293] ^ s[284] ^ s[265] ^ r[284], s[292] ^ s[283] ^ s[264] ^ r[283],
                    s[291] ^ s[282] ^ s[263] ^ r[282], s[290] ^ s[281] ^ s[262] ^ r[281], s[289] ^ s[280] ^ s[261] ^ r[280], s[288] ^ s[279] ^ s[260] ^ r[279], s[287] ^ s[278] ^ s[259] ^ r[278], s[286] ^ s[277] ^ s[258] ^ r[277], s[285] ^ s[276] ^ s[257] ^ r[276], s[284] ^ s[275] ^ s[256] ^ r[275]};

        assign Y1 = {s[255] ^ s[252] ^ s[230] ^ r[252], s[254] ^ s[251] ^ s[229] ^ r[251], s[253] ^ s[250] ^ s[228] ^ r[250], s[252] ^ s[249] ^ s[227] ^ r[249], s[251] ^ s[248] ^ s[226] ^ r[248], s[250] ^ s[247] ^ s[225] ^ r[247], s[249] ^ s[246] ^ s[224] ^ r[246], s[248] ^ s[245] ^ s[223] ^ r[245],
                    s[247] ^ s[244] ^ s[222] ^ r[244], s[246] ^ s[243] ^ s[221] ^ r[243], s[245] ^ s[242] ^ s[220] ^ r[242], s[244] ^ s[241] ^ s[219] ^ r[241], s[243] ^ s[240] ^ s[218] ^ r[240], s[242] ^ s[239] ^ s[217] ^ r[239], s[241] ^ s[238] ^ s[216] ^ r[238], s[240] ^ s[237] ^ s[215] ^ r[237],
                    s[239] ^ s[236] ^ s[214] ^ r[236], s[238] ^ s[235] ^ s[213] ^ r[235], s[237] ^ s[234] ^ s[212] ^ r[234], s[236] ^ s[233] ^ s[211] ^ r[233], s[235] ^ s[232] ^ s[210] ^ r[232], s[234] ^ s[231] ^ s[209] ^ r[231], s[233] ^ s[230] ^ s[208] ^ r[230], s[232] ^ s[229] ^ s[207] ^ r[229],
                    s[231] ^ s[228] ^ s[206] ^ r[228], s[230] ^ s[227] ^ s[205] ^ r[227], s[229] ^ s[226] ^ s[204] ^ r[226], s[228] ^ s[225] ^ s[203] ^ r[225], s[227] ^ s[224] ^ s[202] ^ r[224], s[226] ^ s[223] ^ s[201] ^ r[223], s[225] ^ s[222] ^ s[200] ^ r[222], s[224] ^ s[221] ^ s[199] ^ r[221],
                    s[223] ^ s[220] ^ s[198] ^ r[220], s[222] ^ s[219] ^ s[197] ^ r[219], s[221] ^ s[218] ^ s[196] ^ r[218], s[220] ^ s[217] ^ s[195] ^ r[217], s[219] ^ s[216] ^ s[194] ^ r[216], s[218] ^ s[215] ^ s[193] ^ r[215], s[217] ^ s[214] ^ s[192] ^ r[214], s[255] ^ s[216] ^ s[213] ^ r[216],
                    s[254] ^ s[215] ^ s[212] ^ r[215], s[253] ^ s[214] ^ s[211] ^ r[214], s[252] ^ s[213] ^ s[210] ^ r[213], s[251] ^ s[212] ^ s[209] ^ r[212], s[250] ^ s[211] ^ s[208] ^ r[211], s[249] ^ s[210] ^ s[207] ^ r[210], s[248] ^ s[209] ^ s[206] ^ r[209], s[247] ^ s[208] ^ s[205] ^ r[208],
                    s[246] ^ s[207] ^ s[204] ^ r[207], s[245] ^ s[206] ^ s[203] ^ r[206], s[244] ^ s[205] ^ s[202] ^ r[205], s[243] ^ s[204] ^ s[201] ^ r[204], s[242] ^ s[203] ^ s[200] ^ r[203], s[241] ^ s[202] ^ s[199] ^ r[202], s[240] ^ s[201] ^ s[198] ^ r[201], s[239] ^ s[200] ^ s[197] ^ r[200],
                    s[238] ^ s[199] ^ s[196] ^ r[199], s[237] ^ s[198] ^ s[195] ^ r[198], s[236] ^ s[197] ^ s[194] ^ r[197], s[235] ^ s[196] ^ s[193] ^ r[196], s[234] ^ s[195] ^ s[192] ^ r[195], s[255] ^ s[233] ^ s[194] ^ r[233], s[254] ^ s[232] ^ s[193] ^ r[232], s[253] ^ s[231] ^ s[192] ^ r[231]};

        assign Y2 = {s[191] ^ s[133] ^ s[128] ^ r[133], s[191] ^ s[190] ^ s[132] ^ r[190], s[190] ^ s[189] ^ s[131] ^ r[189], s[189] ^ s[188] ^ s[130] ^ r[188], s[188] ^ s[187] ^ s[129] ^ r[187], s[187] ^ s[186] ^ s[128] ^ r[186], s[191] ^ s[186] ^ s[185] ^ r[186], s[190] ^ s[185] ^ s[184] ^ r[185],
                    s[189] ^ s[184] ^ s[183] ^ r[184], s[188] ^ s[183] ^ s[182] ^ r[183], s[187] ^ s[182] ^ s[181] ^ r[182], s[186] ^ s[181] ^ s[180] ^ r[181], s[185] ^ s[180] ^ s[179] ^ r[180], s[184] ^ s[179] ^ s[178] ^ r[179], s[183] ^ s[178] ^ s[177] ^ r[178], s[182] ^ s[177] ^ s[176] ^ r[177],
                    s[181] ^ s[176] ^ s[175] ^ r[176], s[180] ^ s[175] ^ s[174] ^ r[175], s[179] ^ s[174] ^ s[173] ^ r[174], s[178] ^ s[173] ^ s[172] ^ r[173], s[177] ^ s[172] ^ s[171] ^ r[172], s[176] ^ s[171] ^ s[170] ^ r[171], s[175] ^ s[170] ^ s[169] ^ r[170], s[174] ^ s[169] ^ s[168] ^ r[169],
                    s[173] ^ s[168] ^ s[167] ^ r[168], s[172] ^ s[167] ^ s[166] ^ r[167], s[171] ^ s[166] ^ s[165] ^ r[166], s[170] ^ s[165] ^ s[164] ^ r[165], s[169] ^ s[164] ^ s[163] ^ r[164], s[168] ^ s[163] ^ s[162] ^ r[163], s[167] ^ s[162] ^ s[161] ^ r[162], s[166] ^ s[161] ^ s[160] ^ r[161],
                    s[165] ^ s[160] ^ s[159] ^ r[160], s[164] ^ s[159] ^ s[158] ^ r[159], s[163] ^ s[158] ^ s[157] ^ r[158], s[162] ^ s[157] ^ s[156] ^ r[157], s[161] ^ s[156] ^ s[155] ^ r[156], s[160] ^ s[155] ^ s[154] ^ r[155], s[159] ^ s[154] ^ s[153] ^ r[154], s[158] ^ s[153] ^ s[152] ^ r[153],
                    s[157] ^ s[152] ^ s[151] ^ r[152], s[156] ^ s[151] ^ s[150] ^ r[151], s[155] ^ s[150] ^ s[149] ^ r[150], s[154] ^ s[149] ^ s[148] ^ r[149], s[153] ^ s[148] ^ s[147] ^ r[148], s[152] ^ s[147] ^ s[146] ^ r[147], s[151] ^ s[146] ^ s[145] ^ r[146], s[150] ^ s[145] ^ s[144] ^ r[145],
                    s[149] ^ s[144] ^ s[143] ^ r[144], s[148] ^ s[143] ^ s[142] ^ r[143], s[147] ^ s[142] ^ s[141] ^ r[142], s[146] ^ s[141] ^ s[140] ^ r[141], s[145] ^ s[140] ^ s[139] ^ r[140], s[144] ^ s[139] ^ s[138] ^ r[139], s[143] ^ s[138] ^ s[137] ^ r[138], s[142] ^ s[137] ^ s[136] ^ r[137],
                    s[141] ^ s[136] ^ s[135] ^ r[136], s[140] ^ s[135] ^ s[134] ^ r[135], s[139] ^ s[134] ^ s[133] ^ r[134], s[138] ^ s[133] ^ s[132] ^ r[133], s[137] ^ s[132] ^ s[131] ^ r[132], s[136] ^ s[131] ^ s[130] ^ r[131], s[135] ^ s[130] ^ s[129] ^ r[130], s[134] ^ s[129] ^ s[128] ^ r[129]};

        assign Y3 = {s[127] ^ s[80] ^ s[73] ^ r[80], s[126] ^ s[79] ^ s[72] ^ r[79], s[125] ^ s[78] ^ s[71] ^ r[78], s[124] ^ s[77] ^ s[70] ^ r[77], s[123] ^ s[76] ^ s[69] ^ r[76], s[122] ^ s[75] ^ s[68] ^ r[75], s[121] ^ s[74] ^ s[67] ^ r[74], s[120] ^ s[73] ^ s[66] ^ r[73],
                    s[119] ^ s[72] ^ s[65] ^ r[72], s[118] ^ s[71] ^ s[64] ^ r[71], s[127] ^ s[117] ^ s[70] ^ r[117], s[126] ^ s[116] ^ s[69] ^ r[116], s[125] ^ s[115] ^ s[68] ^ r[115], s[124] ^ s[114] ^ s[67] ^ r[114], s[123] ^ s[113] ^ s[66] ^ r[113], s[122] ^ s[112] ^ s[65] ^ r[112],
                    s[121] ^ s[111] ^ s[64] ^ r[111], s[127] ^ s[120] ^ s[110] ^ r[120], s[126] ^ s[119] ^ s[109] ^ r[119], s[125] ^ s[118] ^ s[108] ^ r[118], s[124] ^ s[117] ^ s[107] ^ r[117], s[123] ^ s[116] ^ s[106] ^ r[116], s[122] ^ s[115] ^ s[105] ^ r[115], s[121] ^ s[114] ^ s[104] ^ r[114],
                    s[120] ^ s[113] ^ s[103] ^ r[113], s[119] ^ s[112] ^ s[102] ^ r[112], s[118] ^ s[111] ^ s[101] ^ r[111], s[117] ^ s[110] ^ s[100] ^ r[110], s[116] ^ s[109] ^ s[99] ^ r[109], s[115] ^ s[108] ^ s[98] ^ r[108], s[114] ^ s[107] ^ s[97] ^ r[107], s[113] ^ s[106] ^ s[96] ^ r[106],
                    s[112] ^ s[105] ^ s[95] ^ r[105], s[111] ^ s[104] ^ s[94] ^ r[104], s[110] ^ s[103] ^ s[93] ^ r[103], s[109] ^ s[102] ^ s[92] ^ r[102], s[108] ^ s[101] ^ s[91] ^ r[101], s[107] ^ s[100] ^ s[90] ^ r[100], s[106] ^ s[99] ^ s[89] ^ r[99], s[105] ^ s[98] ^ s[88] ^ r[98],
                    s[104] ^ s[97] ^ s[87] ^ r[97], s[103] ^ s[96] ^ s[86] ^ r[96], s[102] ^ s[95] ^ s[85] ^ r[95], s[101] ^ s[94] ^ s[84] ^ r[94], s[100] ^ s[93] ^ s[83] ^ r[93], s[99] ^ s[92] ^ s[82] ^ r[92], s[98] ^ s[91] ^ s[81] ^ r[91], s[97] ^ s[90] ^ s[80] ^ r[90],
                    s[96] ^ s[89] ^ s[79] ^ r[89], s[95] ^ s[88] ^ s[78] ^ r[88], s[94] ^ s[87] ^ s[77] ^ r[87], s[93] ^ s[86] ^ s[76] ^ r[86], s[92] ^ s[85] ^ s[75] ^ r[85], s[91] ^ s[84] ^ s[74] ^ r[84], s[90] ^ s[83] ^ s[73] ^ r[83], s[89] ^ s[82] ^ s[72] ^ r[82],
                    s[88] ^ s[81] ^ s[71] ^ r[81], s[87] ^ s[80] ^ s[70] ^ r[80], s[86] ^ s[79] ^ s[69] ^ r[79], s[85] ^ s[78] ^ s[68] ^ r[78], s[84] ^ s[77] ^ s[67] ^ r[77], s[83] ^ s[76] ^ s[66] ^ r[76], s[82] ^ s[75] ^ s[65] ^ r[75], s[81] ^ s[74] ^ s[64] ^ r[74]};

        assign Y4 = {s[63] ^ s[40] ^ s[6] ^ r[40], s[62] ^ s[39] ^ s[5] ^ r[39], s[61] ^ s[38] ^ s[4] ^ r[38], s[60] ^ s[37] ^ s[3] ^ r[37], s[59] ^ s[36] ^ s[2] ^ r[36], s[58] ^ s[35] ^ s[1] ^ r[35], s[57] ^ s[34] ^ s[0] ^ r[34], s[63] ^ s[56] ^ s[33] ^ r[56],
                    s[62] ^ s[55] ^ s[32] ^ r[55], s[61] ^ s[54] ^ s[31] ^ r[54], s[60] ^ s[53] ^ s[30] ^ r[53], s[59] ^ s[52] ^ s[29] ^ r[52], s[58] ^ s[51] ^ s[28] ^ r[51], s[57] ^ s[50] ^ s[27] ^ r[50], s[56] ^ s[49] ^ s[26] ^ r[49], s[55] ^ s[48] ^ s[25] ^ r[48],
                    s[54] ^ s[47] ^ s[24] ^ r[47], s[53] ^ s[46] ^ s[23] ^ r[46], s[52] ^ s[45] ^ s[22] ^ r[45], s[51] ^ s[44] ^ s[21] ^ r[44], s[50] ^ s[43] ^ s[20] ^ r[43], s[49] ^ s[42] ^ s[19] ^ r[42], s[48] ^ s[41] ^ s[18] ^ r[41], s[47] ^ s[40] ^ s[17] ^ r[40],
                    s[46] ^ s[39] ^ s[16] ^ r[39], s[45] ^ s[38] ^ s[15] ^ r[38], s[44] ^ s[37] ^ s[14] ^ r[37], s[43] ^ s[36] ^ s[13] ^ r[36], s[42] ^ s[35] ^ s[12] ^ r[35], s[41] ^ s[34] ^ s[11] ^ r[34], s[40] ^ s[33] ^ s[10] ^ r[33], s[39] ^ s[32] ^ s[9] ^ r[32],
                    s[38] ^ s[31] ^ s[8] ^ r[31], s[37] ^ s[30] ^ s[7] ^ r[30], s[36] ^ s[29] ^ s[6] ^ r[29], s[35] ^ s[28] ^ s[5] ^ r[28], s[34] ^ s[27] ^ s[4] ^ r[27], s[33] ^ s[26] ^ s[3] ^ r[26], s[32] ^ s[25] ^ s[2] ^ r[25], s[31] ^ s[24] ^ s[1] ^ r[24],
                    s[30] ^ s[23] ^ s[0] ^ r[23], s[63] ^ s[29] ^ s[22] ^ r[29], s[62] ^ s[28] ^ s[21] ^ r[28], s[61] ^ s[27] ^ s[20] ^ r[27], s[60] ^ s[26] ^ s[19] ^ r[26], s[59] ^ s[25] ^ s[18] ^ r[25], s[58] ^ s[24] ^ s[17] ^ r[24], s[57] ^ s[23] ^ s[16] ^ r[23],
                    s[56] ^ s[22] ^ s[15] ^ r[22], s[55] ^ s[21] ^ s[14] ^ r[21], s[54] ^ s[20] ^ s[13] ^ r[20], s[53] ^ s[19] ^ s[12] ^ r[19], s[52] ^ s[18] ^ s[11] ^ r[18], s[51] ^ s[17] ^ s[10] ^ r[17], s[50] ^ s[16] ^ s[9] ^ r[16], s[49] ^ s[15] ^ s[8] ^ r[15],
                    s[48] ^ s[14] ^ s[7] ^ r[14], s[47] ^ s[13] ^ s[6] ^ r[13], s[46] ^ s[12] ^ s[5] ^ r[12], s[45] ^ s[11] ^ s[4] ^ r[11], s[44] ^ s[10] ^ s[3] ^ r[10], s[43] ^ s[9] ^ s[2] ^ r[9], s[42] ^ s[8] ^ s[1] ^ r[8], s[41] ^ s[7] ^ s[0] ^ r[7]};

endmodule

module linear_layer_2 (
    input [63:0] x0, x1, x2, x3, x4,
    input [63:0] r0, r1, r2, r3, r4,
    output [63:0] Y0, Y1, Y2, Y3, Y4
);


    // Matrix multiplication algorithm

        wire [319:0] s,r;
        assign s = {x0,x1,x2,x3,x4};
        assign r = {r0,r1,r2,r3,r4};

        assign Y0 = {s[319] ^ s[283] ^ s[274] ^ r[283] ^ r[319], s[318] ^ s[282] ^ s[273] ^ r[282] ^ r[318], s[317] ^ s[281] ^ s[272] ^ r[281] ^ r[317], s[316] ^ s[280] ^ s[271] ^ r[280] ^ r[316], s[315] ^ s[279] ^ s[270] ^ r[279] ^ r[315], s[314] ^ s[278] ^ s[269] ^ r[278] ^ r[314], s[313] ^ s[277] ^ s[268] ^ r[277] ^ r[313], s[312] ^ s[276] ^ s[267] ^ r[276] ^ r[312],
                    s[311] ^ s[275] ^ s[266] ^ r[275] ^ r[311], s[310] ^ s[274] ^ s[265] ^ r[274] ^ r[310], s[309] ^ s[273] ^ s[264] ^ r[273] ^ r[309], s[308] ^ s[272] ^ s[263] ^ r[272] ^ r[308], s[307] ^ s[271] ^ s[262] ^ r[271] ^ r[307], s[306] ^ s[270] ^ s[261] ^ r[270] ^ r[306], s[305] ^ s[269] ^ s[260] ^ r[269] ^ r[305], s[304] ^ s[268] ^ s[259] ^ r[268] ^ r[304],
                    s[303] ^ s[267] ^ s[258] ^ r[267] ^ r[303], s[302] ^ s[266] ^ s[257] ^ r[266] ^ r[302], s[301] ^ s[265] ^ s[256] ^ r[265] ^ r[301], s[319] ^ s[300] ^ s[264] ^ r[300] ^ r[319], s[318] ^ s[299] ^ s[263] ^ r[299] ^ r[318], s[317] ^ s[298] ^ s[262] ^ r[298] ^ r[317], s[316] ^ s[297] ^ s[261] ^ r[297] ^ r[316], s[315] ^ s[296] ^ s[260] ^ r[296] ^ r[315],
                    s[314] ^ s[295] ^ s[259] ^ r[295] ^ r[314], s[313] ^ s[294] ^ s[258] ^ r[294] ^ r[313], s[312] ^ s[293] ^ s[257] ^ r[293] ^ r[312], s[311] ^ s[292] ^ s[256] ^ r[292] ^ r[311], s[319] ^ s[310] ^ s[291] ^ r[310] ^ r[319], s[318] ^ s[309] ^ s[290] ^ r[309] ^ r[318], s[317] ^ s[308] ^ s[289] ^ r[308] ^ r[317], s[316] ^ s[307] ^ s[288] ^ r[307] ^ r[316],
                    s[315] ^ s[306] ^ s[287] ^ r[306] ^ r[315], s[314] ^ s[305] ^ s[286] ^ r[305] ^ r[314], s[313] ^ s[304] ^ s[285] ^ r[304] ^ r[313], s[312] ^ s[303] ^ s[284] ^ r[303] ^ r[312], s[311] ^ s[302] ^ s[283] ^ r[302] ^ r[311], s[310] ^ s[301] ^ s[282] ^ r[301] ^ r[310], s[309] ^ s[300] ^ s[281] ^ r[300] ^ r[309], s[308] ^ s[299] ^ s[280] ^ r[299] ^ r[308],
                    s[307] ^ s[298] ^ s[279] ^ r[298] ^ r[307], s[306] ^ s[297] ^ s[278] ^ r[297] ^ r[306], s[305] ^ s[296] ^ s[277] ^ r[296] ^ r[305], s[304] ^ s[295] ^ s[276] ^ r[295] ^ r[304], s[303] ^ s[294] ^ s[275] ^ r[294] ^ r[303], s[302] ^ s[293] ^ s[274] ^ r[293] ^ r[302], s[301] ^ s[292] ^ s[273] ^ r[292] ^ r[301], s[300] ^ s[291] ^ s[272] ^ r[291] ^ r[300],
                    s[299] ^ s[290] ^ s[271] ^ r[290] ^ r[299], s[298] ^ s[289] ^ s[270] ^ r[289] ^ r[298], s[297] ^ s[288] ^ s[269] ^ r[288] ^ r[297], s[296] ^ s[287] ^ s[268] ^ r[287] ^ r[296], s[295] ^ s[286] ^ s[267] ^ r[286] ^ r[295], s[294] ^ s[285] ^ s[266] ^ r[285] ^ r[294], s[293] ^ s[284] ^ s[265] ^ r[284] ^ r[293], s[292] ^ s[283] ^ s[264] ^ r[283] ^ r[292],
                    s[291] ^ s[282] ^ s[263] ^ r[282] ^ r[291], s[290] ^ s[281] ^ s[262] ^ r[281] ^ r[290], s[289] ^ s[280] ^ s[261] ^ r[280] ^ r[289], s[288] ^ s[279] ^ s[260] ^ r[279] ^ r[288], s[287] ^ s[278] ^ s[259] ^ r[278] ^ r[287], s[286] ^ s[277] ^ s[258] ^ r[277] ^ r[286], s[285] ^ s[276] ^ s[257] ^ r[276] ^ r[285], s[284] ^ s[275] ^ s[256] ^ r[275] ^ r[284]};

        assign Y1 = {s[255] ^ s[252] ^ s[230] ^ r[252] ^ r[255], s[254] ^ s[251] ^ s[229] ^ r[251] ^ r[254], s[253] ^ s[250] ^ s[228] ^ r[250] ^ r[253], s[252] ^ s[249] ^ s[227] ^ r[249] ^ r[252], s[251] ^ s[248] ^ s[226] ^ r[248] ^ r[251], s[250] ^ s[247] ^ s[225] ^ r[247] ^ r[250], s[249] ^ s[246] ^ s[224] ^ r[246] ^ r[249], s[248] ^ s[245] ^ s[223] ^ r[245] ^ r[248],
                    s[247] ^ s[244] ^ s[222] ^ r[244] ^ r[247], s[246] ^ s[243] ^ s[221] ^ r[243] ^ r[246], s[245] ^ s[242] ^ s[220] ^ r[242] ^ r[245], s[244] ^ s[241] ^ s[219] ^ r[241] ^ r[244], s[243] ^ s[240] ^ s[218] ^ r[240] ^ r[243], s[242] ^ s[239] ^ s[217] ^ r[239] ^ r[242], s[241] ^ s[238] ^ s[216] ^ r[238] ^ r[241], s[240] ^ s[237] ^ s[215] ^ r[237] ^ r[240],
                    s[239] ^ s[236] ^ s[214] ^ r[236] ^ r[239], s[238] ^ s[235] ^ s[213] ^ r[235] ^ r[238], s[237] ^ s[234] ^ s[212] ^ r[234] ^ r[237], s[236] ^ s[233] ^ s[211] ^ r[233] ^ r[236], s[235] ^ s[232] ^ s[210] ^ r[232] ^ r[235], s[234] ^ s[231] ^ s[209] ^ r[231] ^ r[234], s[233] ^ s[230] ^ s[208] ^ r[230] ^ r[233], s[232] ^ s[229] ^ s[207] ^ r[229] ^ r[232],
                    s[231] ^ s[228] ^ s[206] ^ r[228] ^ r[231], s[230] ^ s[227] ^ s[205] ^ r[227] ^ r[230], s[229] ^ s[226] ^ s[204] ^ r[226] ^ r[229], s[228] ^ s[225] ^ s[203] ^ r[225] ^ r[228], s[227] ^ s[224] ^ s[202] ^ r[224] ^ r[227], s[226] ^ s[223] ^ s[201] ^ r[223] ^ r[226], s[225] ^ s[222] ^ s[200] ^ r[222] ^ r[225], s[224] ^ s[221] ^ s[199] ^ r[221] ^ r[224],
                    s[223] ^ s[220] ^ s[198] ^ r[220] ^ r[223], s[222] ^ s[219] ^ s[197] ^ r[219] ^ r[222], s[221] ^ s[218] ^ s[196] ^ r[218] ^ r[221], s[220] ^ s[217] ^ s[195] ^ r[217] ^ r[220], s[219] ^ s[216] ^ s[194] ^ r[216] ^ r[219], s[218] ^ s[215] ^ s[193] ^ r[215] ^ r[218], s[217] ^ s[214] ^ s[192] ^ r[214] ^ r[217], s[255] ^ s[216] ^ s[213] ^ r[216] ^ r[255],
                    s[254] ^ s[215] ^ s[212] ^ r[215] ^ r[254], s[253] ^ s[214] ^ s[211] ^ r[214] ^ r[253], s[252] ^ s[213] ^ s[210] ^ r[213] ^ r[252], s[251] ^ s[212] ^ s[209] ^ r[212] ^ r[251], s[250] ^ s[211] ^ s[208] ^ r[211] ^ r[250], s[249] ^ s[210] ^ s[207] ^ r[210] ^ r[249], s[248] ^ s[209] ^ s[206] ^ r[209] ^ r[248], s[247] ^ s[208] ^ s[205] ^ r[208] ^ r[247],
                    s[246] ^ s[207] ^ s[204] ^ r[207] ^ r[246], s[245] ^ s[206] ^ s[203] ^ r[206] ^ r[245], s[244] ^ s[205] ^ s[202] ^ r[205] ^ r[244], s[243] ^ s[204] ^ s[201] ^ r[204] ^ r[243], s[242] ^ s[203] ^ s[200] ^ r[203] ^ r[242], s[241] ^ s[202] ^ s[199] ^ r[202] ^ r[241], s[240] ^ s[201] ^ s[198] ^ r[201] ^ r[240], s[239] ^ s[200] ^ s[197] ^ r[200] ^ r[239],
                    s[238] ^ s[199] ^ s[196] ^ r[199] ^ r[238], s[237] ^ s[198] ^ s[195] ^ r[198] ^ r[237], s[236] ^ s[197] ^ s[194] ^ r[197] ^ r[236], s[235] ^ s[196] ^ s[193] ^ r[196] ^ r[235], s[234] ^ s[195] ^ s[192] ^ r[195] ^ r[234], s[255] ^ s[233] ^ s[194] ^ r[233] ^ r[255], s[254] ^ s[232] ^ s[193] ^ r[232] ^ r[254], s[253] ^ s[231] ^ s[192] ^ r[231] ^ r[253]};

        assign Y2 = {s[191] ^ s[133] ^ s[128] ^ r[133] ^ r[191], s[191] ^ s[190] ^ s[132] ^ r[190] ^ r[191], s[190] ^ s[189] ^ s[131] ^ r[189] ^ r[190], s[189] ^ s[188] ^ s[130] ^ r[188] ^ r[189], s[188] ^ s[187] ^ s[129] ^ r[187] ^ r[188], s[187] ^ s[186] ^ s[128] ^ r[186] ^ r[187], s[191] ^ s[186] ^ s[185] ^ r[186] ^ r[191], s[190] ^ s[185] ^ s[184] ^ r[185] ^ r[190],
                    s[189] ^ s[184] ^ s[183] ^ r[184] ^ r[189], s[188] ^ s[183] ^ s[182] ^ r[183] ^ r[188], s[187] ^ s[182] ^ s[181] ^ r[182] ^ r[187], s[186] ^ s[181] ^ s[180] ^ r[181] ^ r[186], s[185] ^ s[180] ^ s[179] ^ r[180] ^ r[185], s[184] ^ s[179] ^ s[178] ^ r[179] ^ r[184], s[183] ^ s[178] ^ s[177] ^ r[178] ^ r[183], s[182] ^ s[177] ^ s[176] ^ r[177] ^ r[182],
                    s[181] ^ s[176] ^ s[175] ^ r[176] ^ r[181], s[180] ^ s[175] ^ s[174] ^ r[175] ^ r[180], s[179] ^ s[174] ^ s[173] ^ r[174] ^ r[179], s[178] ^ s[173] ^ s[172] ^ r[173] ^ r[178], s[177] ^ s[172] ^ s[171] ^ r[172] ^ r[177], s[176] ^ s[171] ^ s[170] ^ r[171] ^ r[176], s[175] ^ s[170] ^ s[169] ^ r[170] ^ r[175], s[174] ^ s[169] ^ s[168] ^ r[169] ^ r[174],
                    s[173] ^ s[168] ^ s[167] ^ r[168] ^ r[173], s[172] ^ s[167] ^ s[166] ^ r[167] ^ r[172], s[171] ^ s[166] ^ s[165] ^ r[166] ^ r[171], s[170] ^ s[165] ^ s[164] ^ r[165] ^ r[170], s[169] ^ s[164] ^ s[163] ^ r[164] ^ r[169], s[168] ^ s[163] ^ s[162] ^ r[163] ^ r[168], s[167] ^ s[162] ^ s[161] ^ r[162] ^ r[167], s[166] ^ s[161] ^ s[160] ^ r[161] ^ r[166],
                    s[165] ^ s[160] ^ s[159] ^ r[160] ^ r[165], s[164] ^ s[159] ^ s[158] ^ r[159] ^ r[164], s[163] ^ s[158] ^ s[157] ^ r[158] ^ r[163], s[162] ^ s[157] ^ s[156] ^ r[157] ^ r[162], s[161] ^ s[156] ^ s[155] ^ r[156] ^ r[161], s[160] ^ s[155] ^ s[154] ^ r[155] ^ r[160], s[159] ^ s[154] ^ s[153] ^ r[154] ^ r[159], s[158] ^ s[153] ^ s[152] ^ r[153] ^ r[158],
                    s[157] ^ s[152] ^ s[151] ^ r[152] ^ r[157], s[156] ^ s[151] ^ s[150] ^ r[151] ^ r[156], s[155] ^ s[150] ^ s[149] ^ r[150] ^ r[155], s[154] ^ s[149] ^ s[148] ^ r[149] ^ r[154], s[153] ^ s[148] ^ s[147] ^ r[148] ^ r[153], s[152] ^ s[147] ^ s[146] ^ r[147] ^ r[152], s[151] ^ s[146] ^ s[145] ^ r[146] ^ r[151], s[150] ^ s[145] ^ s[144] ^ r[145] ^ r[150],
                    s[149] ^ s[144] ^ s[143] ^ r[144] ^ r[149], s[148] ^ s[143] ^ s[142] ^ r[143] ^ r[148], s[147] ^ s[142] ^ s[141] ^ r[142] ^ r[147], s[146] ^ s[141] ^ s[140] ^ r[141] ^ r[146], s[145] ^ s[140] ^ s[139] ^ r[140] ^ r[145], s[144] ^ s[139] ^ s[138] ^ r[139] ^ r[144], s[143] ^ s[138] ^ s[137] ^ r[138] ^ r[143], s[142] ^ s[137] ^ s[136] ^ r[137] ^ r[142],
                    s[141] ^ s[136] ^ s[135] ^ r[136] ^ r[141], s[140] ^ s[135] ^ s[134] ^ r[135] ^ r[140], s[139] ^ s[134] ^ s[133] ^ r[134] ^ r[139], s[138] ^ s[133] ^ s[132] ^ r[133] ^ r[138], s[137] ^ s[132] ^ s[131] ^ r[132] ^ r[137], s[136] ^ s[131] ^ s[130] ^ r[131] ^ r[136], s[135] ^ s[130] ^ s[129] ^ r[130] ^ r[135], s[134] ^ s[129] ^ s[128] ^ r[129] ^ r[134]};

        assign Y3 = {s[127] ^ s[80] ^ s[73] ^ r[80] ^ r[127], s[126] ^ s[79] ^ s[72] ^ r[79] ^ r[126], s[125] ^ s[78] ^ s[71] ^ r[78] ^ r[125], s[124] ^ s[77] ^ s[70] ^ r[77] ^ r[124], s[123] ^ s[76] ^ s[69] ^ r[76] ^ r[123], s[122] ^ s[75] ^ s[68] ^ r[75] ^ r[122], s[121] ^ s[74] ^ s[67] ^ r[74] ^ r[121], s[120] ^ s[73] ^ s[66] ^ r[73] ^ r[120],
                    s[119] ^ s[72] ^ s[65] ^ r[72] ^ r[119], s[118] ^ s[71] ^ s[64] ^ r[71] ^ r[118], s[127] ^ s[117] ^ s[70] ^ r[117] ^ r[127], s[126] ^ s[116] ^ s[69] ^ r[116] ^ r[126], s[125] ^ s[115] ^ s[68] ^ r[115] ^ r[125], s[124] ^ s[114] ^ s[67] ^ r[114] ^ r[124], s[123] ^ s[113] ^ s[66] ^ r[113] ^ r[123], s[122] ^ s[112] ^ s[65] ^ r[112] ^ r[122],
                    s[121] ^ s[111] ^ s[64] ^ r[111] ^ r[121], s[127] ^ s[120] ^ s[110] ^ r[120] ^ r[127], s[126] ^ s[119] ^ s[109] ^ r[119] ^ r[126], s[125] ^ s[118] ^ s[108] ^ r[118] ^ r[125], s[124] ^ s[117] ^ s[107] ^ r[117] ^ r[124], s[123] ^ s[116] ^ s[106] ^ r[116] ^ r[123], s[122] ^ s[115] ^ s[105] ^ r[115] ^ r[122], s[121] ^ s[114] ^ s[104] ^ r[114] ^ r[121],
                    s[120] ^ s[113] ^ s[103] ^ r[113] ^ r[120], s[119] ^ s[112] ^ s[102] ^ r[112] ^ r[119], s[118] ^ s[111] ^ s[101] ^ r[111] ^ r[118], s[117] ^ s[110] ^ s[100] ^ r[110] ^ r[117], s[116] ^ s[109] ^ s[99] ^ r[109] ^ r[116], s[115] ^ s[108] ^ s[98] ^ r[108] ^ r[115], s[114] ^ s[107] ^ s[97] ^ r[107] ^ r[114], s[113] ^ s[106] ^ s[96] ^ r[106] ^ r[113],
                    s[112] ^ s[105] ^ s[95] ^ r[105] ^ r[112], s[111] ^ s[104] ^ s[94] ^ r[104] ^ r[111], s[110] ^ s[103] ^ s[93] ^ r[103] ^ r[110], s[109] ^ s[102] ^ s[92] ^ r[102] ^ r[109], s[108] ^ s[101] ^ s[91] ^ r[101] ^ r[108], s[107] ^ s[100] ^ s[90] ^ r[100] ^ r[107], s[106] ^ s[99] ^ s[89] ^ r[99] ^ r[106], s[105] ^ s[98] ^ s[88] ^ r[98] ^ r[105],
                    s[104] ^ s[97] ^ s[87] ^ r[97] ^ r[104], s[103] ^ s[96] ^ s[86] ^ r[96] ^ r[103], s[102] ^ s[95] ^ s[85] ^ r[95] ^ r[102], s[101] ^ s[94] ^ s[84] ^ r[94] ^ r[101], s[100] ^ s[93] ^ s[83] ^ r[93] ^ r[100], s[99] ^ s[92] ^ s[82] ^ r[92] ^ r[99], s[98] ^ s[91] ^ s[81] ^ r[91] ^ r[98], s[97] ^ s[90] ^ s[80] ^ r[90] ^ r[97],
                    s[96] ^ s[89] ^ s[79] ^ r[89] ^ r[96], s[95] ^ s[88] ^ s[78] ^ r[88] ^ r[95], s[94] ^ s[87] ^ s[77] ^ r[87] ^ r[94], s[93] ^ s[86] ^ s[76] ^ r[86] ^ r[93], s[92] ^ s[85] ^ s[75] ^ r[85] ^ r[92], s[91] ^ s[84] ^ s[74] ^ r[84] ^ r[91], s[90] ^ s[83] ^ s[73] ^ r[83] ^ r[90], s[89] ^ s[82] ^ s[72] ^ r[82] ^ r[89],
                    s[88] ^ s[81] ^ s[71] ^ r[81] ^ r[88], s[87] ^ s[80] ^ s[70] ^ r[80] ^ r[87], s[86] ^ s[79] ^ s[69] ^ r[79] ^ r[86], s[85] ^ s[78] ^ s[68] ^ r[78] ^ r[85], s[84] ^ s[77] ^ s[67] ^ r[77] ^ r[84], s[83] ^ s[76] ^ s[66] ^ r[76] ^ r[83], s[82] ^ s[75] ^ s[65] ^ r[75] ^ r[82], s[81] ^ s[74] ^ s[64] ^ r[74] ^ r[81]};

        assign Y4 = {s[63] ^ s[40] ^ s[6] ^ r[40] ^ r[63], s[62] ^ s[39] ^ s[5] ^ r[39] ^ r[62], s[61] ^ s[38] ^ s[4] ^ r[38] ^ r[61], s[60] ^ s[37] ^ s[3] ^ r[37] ^ r[60], s[59] ^ s[36] ^ s[2] ^ r[36] ^ r[59], s[58] ^ s[35] ^ s[1] ^ r[35] ^ r[58], s[57] ^ s[34] ^ s[0] ^ r[34] ^ r[57], s[63] ^ s[56] ^ s[33] ^ r[56] ^ r[63],
                    s[62] ^ s[55] ^ s[32] ^ r[55] ^ r[62], s[61] ^ s[54] ^ s[31] ^ r[54] ^ r[61], s[60] ^ s[53] ^ s[30] ^ r[53] ^ r[60], s[59] ^ s[52] ^ s[29] ^ r[52] ^ r[59], s[58] ^ s[51] ^ s[28] ^ r[51] ^ r[58], s[57] ^ s[50] ^ s[27] ^ r[50] ^ r[57], s[56] ^ s[49] ^ s[26] ^ r[49] ^ r[56], s[55] ^ s[48] ^ s[25] ^ r[48] ^ r[55],
                    s[54] ^ s[47] ^ s[24] ^ r[47] ^ r[54], s[53] ^ s[46] ^ s[23] ^ r[46] ^ r[53], s[52] ^ s[45] ^ s[22] ^ r[45] ^ r[52], s[51] ^ s[44] ^ s[21] ^ r[44] ^ r[51], s[50] ^ s[43] ^ s[20] ^ r[43] ^ r[50], s[49] ^ s[42] ^ s[19] ^ r[42] ^ r[49], s[48] ^ s[41] ^ s[18] ^ r[41] ^ r[48], s[47] ^ s[40] ^ s[17] ^ r[40] ^ r[47],
                    s[46] ^ s[39] ^ s[16] ^ r[39] ^ r[46], s[45] ^ s[38] ^ s[15] ^ r[38] ^ r[45], s[44] ^ s[37] ^ s[14] ^ r[37] ^ r[44], s[43] ^ s[36] ^ s[13] ^ r[36] ^ r[43], s[42] ^ s[35] ^ s[12] ^ r[35] ^ r[42], s[41] ^ s[34] ^ s[11] ^ r[34] ^ r[41], s[40] ^ s[33] ^ s[10] ^ r[33] ^ r[40], s[39] ^ s[32] ^ s[9] ^ r[32] ^ r[39],
                    s[38] ^ s[31] ^ s[8] ^ r[31] ^ r[38], s[37] ^ s[30] ^ s[7] ^ r[30] ^ r[37], s[36] ^ s[29] ^ s[6] ^ r[29] ^ r[36], s[35] ^ s[28] ^ s[5] ^ r[28] ^ r[35], s[34] ^ s[27] ^ s[4] ^ r[27] ^ r[34], s[33] ^ s[26] ^ s[3] ^ r[26] ^ r[33], s[32] ^ s[25] ^ s[2] ^ r[25] ^ r[32], s[31] ^ s[24] ^ s[1] ^ r[24] ^ r[31],
                    s[30] ^ s[23] ^ s[0] ^ r[23] ^ r[30], s[63] ^ s[29] ^ s[22] ^ r[29] ^ r[63], s[62] ^ s[28] ^ s[21] ^ r[28] ^ r[62], s[61] ^ s[27] ^ s[20] ^ r[27] ^ r[61], s[60] ^ s[26] ^ s[19] ^ r[26] ^ r[60], s[59] ^ s[25] ^ s[18] ^ r[25] ^ r[59], s[58] ^ s[24] ^ s[17] ^ r[24] ^ r[58], s[57] ^ s[23] ^ s[16] ^ r[23] ^ r[57],
                    s[56] ^ s[22] ^ s[15] ^ r[22] ^ r[56], s[55] ^ s[21] ^ s[14] ^ r[21] ^ r[55], s[54] ^ s[20] ^ s[13] ^ r[20] ^ r[54], s[53] ^ s[19] ^ s[12] ^ r[19] ^ r[53], s[52] ^ s[18] ^ s[11] ^ r[18] ^ r[52], s[51] ^ s[17] ^ s[10] ^ r[17] ^ r[51], s[50] ^ s[16] ^ s[9] ^ r[16] ^ r[50], s[49] ^ s[15] ^ s[8] ^ r[15] ^ r[49],
                    s[48] ^ s[14] ^ s[7] ^ r[14] ^ r[48], s[47] ^ s[13] ^ s[6] ^ r[13] ^ r[47], s[46] ^ s[12] ^ s[5] ^ r[12] ^ r[46], s[45] ^ s[11] ^ s[4] ^ r[11] ^ r[45], s[44] ^ s[10] ^ s[3] ^ r[10] ^ r[44], s[43] ^ s[9] ^ s[2] ^ r[9] ^ r[43], s[42] ^ s[8] ^ s[1] ^ r[8] ^ r[42], s[41] ^ s[7] ^ s[0] ^ r[7] ^ r[41]};

endmodule