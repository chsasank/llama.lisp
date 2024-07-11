; ModuleID = 'build/MMult1.ll'
source_filename = "build/MMult1.ll"
target triple = "unknown-unknown-unknown"

define void @__MMult1(float* %A, float* %B, float* %C, i32 %m, i32 %n, i32 %k) {
alloca-ghzfqosw:
  %A.1 = alloca float*, align 8
  %B.1 = alloca float*, align 8
  %C.1 = alloca float*, align 8
  %m.1 = alloca i32, align 4
  %n.1 = alloca i32, align 4
  %k.1 = alloca i32, align 4
  %tmp_clisp-okxgnjqj = alloca i32, align 4
  %j = alloca i32, align 4
  %tmp_clisp.inp_0-todqkycp = alloca i32, align 4
  %tmp_clisp.inp_1-whyglieb = alloca i32, align 4
  %tmp_clisp.cond-efmfmgzm = alloca i1, align 1
  %tmp_clisp-yizcryvi = alloca i32, align 4
  %i = alloca i32, align 4
  %tmp_clisp.inp_0-kxxyqfmy = alloca i32, align 4
  %tmp_clisp.inp_1-qubatgbi = alloca i32, align 4
  %tmp_clisp.cond-kdnfpqtg = alloca i1, align 1
  %tmp_clisp-fbdjkvzo = alloca float, align 4
  %sum.jjbnjuet.lyohlpeo = alloca float, align 4
  %tmp_clisp-lnatycbr = alloca i32, align 4
  %p = alloca i32, align 4
  %tmp_clisp.inp_0-ogmqmxtz = alloca i32, align 4
  %tmp_clisp.inp_1-hbjghlxw = alloca i32, align 4
  %tmp_clisp.cond-nrlytcjv = alloca i1, align 1
  %tmp_clisp.inp_0-qxiqvjfo = alloca i32, align 4
  %tmp_clisp.inp_1-wwtfrpap = alloca i32, align 4
  %tmp_clisp.inp_0-jsowcdra = alloca i32, align 4
  %tmp_clisp.inp_1-qztdvgsq = alloca i32, align 4
  %tmp_clisp-zkvgquve = alloca i32, align 4
  %tmp_clisp-tpbrzutg = alloca float*, align 8
  %tmp_clisp.inp_0-dbejwpte = alloca float, align 4
  %tmp_clisp.inp_0-gruyruwz = alloca i32, align 4
  %tmp_clisp.inp_1-qywbibtz = alloca i32, align 4
  %tmp_clisp.inp_0-mmppjmzn = alloca i32, align 4
  %tmp_clisp.inp_1-mgmwophr = alloca i32, align 4
  %tmp_clisp-enppsxio = alloca i32, align 4
  %tmp_clisp-qbdmhuir = alloca float*, align 8
  %tmp_clisp.inp_1-muhwrpxi = alloca float, align 4
  %tmp_clisp.inp_0-tdghqajf = alloca float, align 4
  %tmp_clisp.inp_1-npagwdto = alloca float, align 4
  %tmp_clisp-uudrkpqm = alloca float, align 4
  %tmp_clisp.inp_0-rhowtsgk = alloca i32, align 4
  %tmp_clisp.inp_1-bqtzuquf = alloca i32, align 4
  %tmp_clisp-zjoxezxe = alloca i32, align 4
  %tmp_clisp.inp_0-nukiaxtw = alloca i32, align 4
  %tmp_clisp.inp_1-dpkehhdk = alloca i32, align 4
  %tmp_clisp.inp_0-gfvrblbd = alloca i32, align 4
  %tmp_clisp.inp_1-nimffujb = alloca i32, align 4
  %tmp_clisp-qpodxhwj = alloca i32, align 4
  %tmp_clisp.ptr-ycczouef = alloca float*, align 8
  %tmp_clisp.val-bwhexkam = alloca float, align 4
  %tmp_clisp-eosspqch = alloca float, align 4
  %tmp_clisp.inp_0-apyythbq = alloca i32, align 4
  %tmp_clisp.inp_1-vcolyfgv = alloca i32, align 4
  %tmp_clisp-ozbkwhzf = alloca i32, align 4
  %tmp_clisp.inp_0-vciuftud = alloca i32, align 4
  %tmp_clisp.inp_1-lnfwojxm = alloca i32, align 4
  %tmp_clisp-pyfbpjwl = alloca i32, align 4
  br label %entry-nzsdzimw

entry-nzsdzimw:                                   ; preds = %alloca-ghzfqosw
  store float* %A, float** %A.1, align 8
  store float* %B, float** %B.1, align 8
  store float* %C, float** %C.1, align 8
  store i32 %m, i32* %m.1, align 4
  store i32 %n, i32* %n.1, align 4
  store i32 %k, i32* %k.1, align 4
  store i32 0, i32* %tmp_clisp-okxgnjqj, align 4
  %.15 = load i32, i32* %tmp_clisp-okxgnjqj, align 4
  store i32 %.15, i32* %j, align 4
  br label %tmp_clisp.loop-ixifqpkb

tmp_clisp.loop-ixifqpkb:                          ; preds = %tmp_clisp.break-kjqurocj, %entry-nzsdzimw
  %.18 = load i32, i32* %j, align 4
  store i32 %.18, i32* %tmp_clisp.inp_0-todqkycp, align 4
  %.20 = load i32, i32* %n.1, align 4
  store i32 %.20, i32* %tmp_clisp.inp_1-whyglieb, align 4
  %.22 = load i32, i32* %tmp_clisp.inp_0-todqkycp, align 4
  %.23 = load i32, i32* %tmp_clisp.inp_1-whyglieb, align 4
  %tmp_clisp.cond-efmfmgzm.1 = icmp slt i32 %.22, %.23
  store i1 %tmp_clisp.cond-efmfmgzm.1, i1* %tmp_clisp.cond-efmfmgzm, align 1
  %.25 = load i1, i1* %tmp_clisp.cond-efmfmgzm, align 1
  br i1 %.25, label %tmp_clisp.cont-wqtzzgxu, label %tmp_clisp.break-bcytxtjd

tmp_clisp.cont-wqtzzgxu:                          ; preds = %tmp_clisp.loop-ixifqpkb
  store i32 0, i32* %tmp_clisp-yizcryvi, align 4
  %.28 = load i32, i32* %tmp_clisp-yizcryvi, align 4
  store i32 %.28, i32* %i, align 4
  br label %tmp_clisp.loop-zmajeybt

tmp_clisp.loop-zmajeybt:                          ; preds = %tmp_clisp.break-hucbjbuv, %tmp_clisp.cont-wqtzzgxu
  %.31 = load i32, i32* %i, align 4
  store i32 %.31, i32* %tmp_clisp.inp_0-kxxyqfmy, align 4
  %.33 = load i32, i32* %m.1, align 4
  store i32 %.33, i32* %tmp_clisp.inp_1-qubatgbi, align 4
  %.35 = load i32, i32* %tmp_clisp.inp_0-kxxyqfmy, align 4
  %.36 = load i32, i32* %tmp_clisp.inp_1-qubatgbi, align 4
  %tmp_clisp.cond-kdnfpqtg.1 = icmp slt i32 %.35, %.36
  store i1 %tmp_clisp.cond-kdnfpqtg.1, i1* %tmp_clisp.cond-kdnfpqtg, align 1
  %.38 = load i1, i1* %tmp_clisp.cond-kdnfpqtg, align 1
  br i1 %.38, label %tmp_clisp.cont-mropbssu, label %tmp_clisp.break-kjqurocj

tmp_clisp.cont-mropbssu:                          ; preds = %tmp_clisp.loop-zmajeybt
  store float 0.000000e+00, float* %tmp_clisp-fbdjkvzo, align 4
  %.41 = load float, float* %tmp_clisp-fbdjkvzo, align 4
  store float %.41, float* %sum.jjbnjuet.lyohlpeo, align 4
  store i32 0, i32* %tmp_clisp-lnatycbr, align 4
  %.44 = load i32, i32* %tmp_clisp-lnatycbr, align 4
  store i32 %.44, i32* %p, align 4
  br label %tmp_clisp.loop-fvszfvqt

tmp_clisp.loop-fvszfvqt:                          ; preds = %tmp_clisp.cont-rjfvyedd, %tmp_clisp.cont-mropbssu
  %.47 = load i32, i32* %p, align 4
  store i32 %.47, i32* %tmp_clisp.inp_0-ogmqmxtz, align 4
  %.49 = load i32, i32* %k.1, align 4
  store i32 %.49, i32* %tmp_clisp.inp_1-hbjghlxw, align 4
  %.51 = load i32, i32* %tmp_clisp.inp_0-ogmqmxtz, align 4
  %.52 = load i32, i32* %tmp_clisp.inp_1-hbjghlxw, align 4
  %tmp_clisp.cond-nrlytcjv.1 = icmp slt i32 %.51, %.52
  store i1 %tmp_clisp.cond-nrlytcjv.1, i1* %tmp_clisp.cond-nrlytcjv, align 1
  %.54 = load i1, i1* %tmp_clisp.cond-nrlytcjv, align 1
  br i1 %.54, label %tmp_clisp.cont-rjfvyedd, label %tmp_clisp.break-hucbjbuv

tmp_clisp.cont-rjfvyedd:                          ; preds = %tmp_clisp.loop-fvszfvqt
  %.56 = load i32, i32* %p, align 4
  store i32 %.56, i32* %tmp_clisp.inp_0-qxiqvjfo, align 4
  %.58 = load i32, i32* %m.1, align 4
  store i32 %.58, i32* %tmp_clisp.inp_1-wwtfrpap, align 4
  %.60 = load i32, i32* %tmp_clisp.inp_0-qxiqvjfo, align 4
  %.61 = load i32, i32* %tmp_clisp.inp_1-wwtfrpap, align 4
  %tmp_clisp.inp_0-jsowcdra.1 = mul i32 %.60, %.61
  store i32 %tmp_clisp.inp_0-jsowcdra.1, i32* %tmp_clisp.inp_0-jsowcdra, align 4
  %.63 = load i32, i32* %i, align 4
  store i32 %.63, i32* %tmp_clisp.inp_1-qztdvgsq, align 4
  %.65 = load i32, i32* %tmp_clisp.inp_0-jsowcdra, align 4
  %.66 = load i32, i32* %tmp_clisp.inp_1-qztdvgsq, align 4
  %tmp_clisp-zkvgquve.1 = add i32 %.65, %.66
  store i32 %tmp_clisp-zkvgquve.1, i32* %tmp_clisp-zkvgquve, align 4
  %.68 = load float*, float** %A.1, align 8
  %.69 = load i32, i32* %tmp_clisp-zkvgquve, align 4
  %.70 = getelementptr float, float* %.68, i32 %.69
  store float* %.70, float** %tmp_clisp-tpbrzutg, align 8
  %.72 = load float*, float** %tmp_clisp-tpbrzutg, align 8
  %.73 = load float, float* %.72, align 4
  store float %.73, float* %tmp_clisp.inp_0-dbejwpte, align 4
  %.75 = load i32, i32* %j, align 4
  store i32 %.75, i32* %tmp_clisp.inp_0-gruyruwz, align 4
  %.77 = load i32, i32* %k.1, align 4
  store i32 %.77, i32* %tmp_clisp.inp_1-qywbibtz, align 4
  %.79 = load i32, i32* %tmp_clisp.inp_0-gruyruwz, align 4
  %.80 = load i32, i32* %tmp_clisp.inp_1-qywbibtz, align 4
  %tmp_clisp.inp_0-mmppjmzn.1 = mul i32 %.79, %.80
  store i32 %tmp_clisp.inp_0-mmppjmzn.1, i32* %tmp_clisp.inp_0-mmppjmzn, align 4
  %.82 = load i32, i32* %p, align 4
  store i32 %.82, i32* %tmp_clisp.inp_1-mgmwophr, align 4
  %.84 = load i32, i32* %tmp_clisp.inp_0-mmppjmzn, align 4
  %.85 = load i32, i32* %tmp_clisp.inp_1-mgmwophr, align 4
  %tmp_clisp-enppsxio.1 = add i32 %.84, %.85
  store i32 %tmp_clisp-enppsxio.1, i32* %tmp_clisp-enppsxio, align 4
  %.87 = load float*, float** %B.1, align 8
  %.88 = load i32, i32* %tmp_clisp-enppsxio, align 4
  %.89 = getelementptr float, float* %.87, i32 %.88
  store float* %.89, float** %tmp_clisp-qbdmhuir, align 8
  %.91 = load float*, float** %tmp_clisp-qbdmhuir, align 8
  %.92 = load float, float* %.91, align 4
  store float %.92, float* %tmp_clisp.inp_1-muhwrpxi, align 4
  %.94 = load float, float* %tmp_clisp.inp_0-dbejwpte, align 4
  %.95 = load float, float* %tmp_clisp.inp_1-muhwrpxi, align 4
  %tmp_clisp.inp_0-tdghqajf.1 = fmul float %.94, %.95
  store float %tmp_clisp.inp_0-tdghqajf.1, float* %tmp_clisp.inp_0-tdghqajf, align 4
  %.97 = load float, float* %sum.jjbnjuet.lyohlpeo, align 4
  store float %.97, float* %tmp_clisp.inp_1-npagwdto, align 4
  %.99 = load float, float* %tmp_clisp.inp_0-tdghqajf, align 4
  %.100 = load float, float* %tmp_clisp.inp_1-npagwdto, align 4
  %tmp_clisp-uudrkpqm.1 = fadd float %.99, %.100
  store float %tmp_clisp-uudrkpqm.1, float* %tmp_clisp-uudrkpqm, align 4
  %.102 = load float, float* %tmp_clisp-uudrkpqm, align 4
  store float %.102, float* %sum.jjbnjuet.lyohlpeo, align 4
  %.104 = load i32, i32* %p, align 4
  store i32 %.104, i32* %tmp_clisp.inp_0-rhowtsgk, align 4
  store i32 1, i32* %tmp_clisp.inp_1-bqtzuquf, align 4
  %.107 = load i32, i32* %tmp_clisp.inp_0-rhowtsgk, align 4
  %.108 = load i32, i32* %tmp_clisp.inp_1-bqtzuquf, align 4
  %tmp_clisp-zjoxezxe.1 = add i32 %.107, %.108
  store i32 %tmp_clisp-zjoxezxe.1, i32* %tmp_clisp-zjoxezxe, align 4
  %.110 = load i32, i32* %tmp_clisp-zjoxezxe, align 4
  store i32 %.110, i32* %p, align 4
  br label %tmp_clisp.loop-fvszfvqt

tmp_clisp.break-hucbjbuv:                         ; preds = %tmp_clisp.loop-fvszfvqt
  %.113 = load i32, i32* %j, align 4
  store i32 %.113, i32* %tmp_clisp.inp_0-nukiaxtw, align 4
  %.115 = load i32, i32* %m.1, align 4
  store i32 %.115, i32* %tmp_clisp.inp_1-dpkehhdk, align 4
  %.117 = load i32, i32* %tmp_clisp.inp_0-nukiaxtw, align 4
  %.118 = load i32, i32* %tmp_clisp.inp_1-dpkehhdk, align 4
  %tmp_clisp.inp_0-gfvrblbd.1 = mul i32 %.117, %.118
  store i32 %tmp_clisp.inp_0-gfvrblbd.1, i32* %tmp_clisp.inp_0-gfvrblbd, align 4
  %.120 = load i32, i32* %i, align 4
  store i32 %.120, i32* %tmp_clisp.inp_1-nimffujb, align 4
  %.122 = load i32, i32* %tmp_clisp.inp_0-gfvrblbd, align 4
  %.123 = load i32, i32* %tmp_clisp.inp_1-nimffujb, align 4
  %tmp_clisp-qpodxhwj.1 = add i32 %.122, %.123
  store i32 %tmp_clisp-qpodxhwj.1, i32* %tmp_clisp-qpodxhwj, align 4
  %.125 = load float*, float** %C.1, align 8
  %.126 = load i32, i32* %tmp_clisp-qpodxhwj, align 4
  %.127 = getelementptr float, float* %.125, i32 %.126
  store float* %.127, float** %tmp_clisp.ptr-ycczouef, align 8
  %.129 = load float, float* %sum.jjbnjuet.lyohlpeo, align 4
  store float %.129, float* %tmp_clisp.val-bwhexkam, align 4
  %.131 = load float*, float** %tmp_clisp.ptr-ycczouef, align 8
  %.132 = load float, float* %tmp_clisp.val-bwhexkam, align 4
  store float %.132, float* %.131, align 4
  %.134 = load float, float* %tmp_clisp.val-bwhexkam, align 4
  store float %.134, float* %tmp_clisp-eosspqch, align 4
  %.136 = load i32, i32* %i, align 4
  store i32 %.136, i32* %tmp_clisp.inp_0-apyythbq, align 4
  store i32 1, i32* %tmp_clisp.inp_1-vcolyfgv, align 4
  %.139 = load i32, i32* %tmp_clisp.inp_0-apyythbq, align 4
  %.140 = load i32, i32* %tmp_clisp.inp_1-vcolyfgv, align 4
  %tmp_clisp-ozbkwhzf.1 = add i32 %.139, %.140
  store i32 %tmp_clisp-ozbkwhzf.1, i32* %tmp_clisp-ozbkwhzf, align 4
  %.142 = load i32, i32* %tmp_clisp-ozbkwhzf, align 4
  store i32 %.142, i32* %i, align 4
  br label %tmp_clisp.loop-zmajeybt

tmp_clisp.break-kjqurocj:                         ; preds = %tmp_clisp.loop-zmajeybt
  %.145 = load i32, i32* %j, align 4
  store i32 %.145, i32* %tmp_clisp.inp_0-vciuftud, align 4
  store i32 1, i32* %tmp_clisp.inp_1-lnfwojxm, align 4
  %.148 = load i32, i32* %tmp_clisp.inp_0-vciuftud, align 4
  %.149 = load i32, i32* %tmp_clisp.inp_1-lnfwojxm, align 4
  %tmp_clisp-pyfbpjwl.1 = add i32 %.148, %.149
  store i32 %tmp_clisp-pyfbpjwl.1, i32* %tmp_clisp-pyfbpjwl, align 4
  %.151 = load i32, i32* %tmp_clisp-pyfbpjwl, align 4
  store i32 %.151, i32* %j, align 4
  br label %tmp_clisp.loop-ixifqpkb

tmp_clisp.break-bcytxtjd:                         ; preds = %tmp_clisp.loop-ixifqpkb
  br label %tmp_clisp.ret_lbl-eadzrstg

tmp_clisp.ret_lbl-eadzrstg:                       ; preds = %tmp_clisp.break-bcytxtjd
  ret void
}
