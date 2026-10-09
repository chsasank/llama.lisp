; ModuleID = ""
target datalayout = "e-i64:64-v16:16-v24:32-v32:32-v48:64-v96:128-v192:256-v256:256-v512:512-v1024:1024-n8:16:32:64-G1"
target triple = "spir64-unknown-unknown"

declare spir_func i64 @"_Z33__spirv_BuiltInGlobalInvocationIdi"(i32 %".1") #1

define spir_kernel void @"kernel"(i32 addrspace(1)* %"sum", i32 addrspace(1)* %"a", i32 addrspace(1)* %"b") #0
{
alloca-piuzfbqp:
  %"sum.1" = alloca i32 addrspace(1)*
  %"a.1" = alloca i32 addrspace(1)*
  %"b.1" = alloca i32 addrspace(1)*
  %"i" = alloca i64
  %"tmp_clisp-fvjvleoy" = alloca i32
  %"tmp_clisp-hotmbsah" = alloca i64
  %"tmp_clisp-egjrlyhk" = alloca i32 addrspace(1)*
  %"tmp_clisp-vronpcus" = alloca i32 addrspace(1)*
  %"tmp_clisp-kqfhnhbb" = alloca i32
  %"tmp_clisp-pjtvcrce" = alloca i32 addrspace(1)*
  %"tmp_clisp-msltedyy" = alloca i32
  %"tmp_clisp-dofzglnn" = alloca i32
  br label %"entry-sbnpsago"
entry-sbnpsago:
  store i32 addrspace(1)* %"sum", i32 addrspace(1)** %"sum.1"
  store i32 addrspace(1)* %"a", i32 addrspace(1)** %"a.1"
  store i32 addrspace(1)* %"b", i32 addrspace(1)** %"b.1"
  store i64 undef, i64* %"i"
  store i32 0, i32* %"tmp_clisp-fvjvleoy"
  %".10" = load i32, i32* %"tmp_clisp-fvjvleoy"
  %"tmp_clisp-hotmbsah.1" = call i64 @"_Z33__spirv_BuiltInGlobalInvocationIdi"(i32 %".10")
  store i64 %"tmp_clisp-hotmbsah.1", i64* %"tmp_clisp-hotmbsah"
  %".12" = load i64, i64* %"tmp_clisp-hotmbsah"
  store i64 %".12", i64* %"i"
  %".14" = load i64, i64* %"i"
  %".15" = load i32 addrspace(1)*, i32 addrspace(1)** %"sum.1"
  %".16" = getelementptr i32, i32 addrspace(1)* %".15", i64 %".14"
  store i32 addrspace(1)* %".16", i32 addrspace(1)** %"tmp_clisp-egjrlyhk"
  %".18" = load i64, i64* %"i"
  %".19" = load i32 addrspace(1)*, i32 addrspace(1)** %"a.1"
  %".20" = getelementptr i32, i32 addrspace(1)* %".19", i64 %".18"
  store i32 addrspace(1)* %".20", i32 addrspace(1)** %"tmp_clisp-vronpcus"
  %".22" = load i32 addrspace(1)*, i32 addrspace(1)** %"tmp_clisp-vronpcus"
  %".23" = load i32, i32 addrspace(1)* %".22", !invariant.load !0
  store i32 %".23", i32* %"tmp_clisp-kqfhnhbb"
  %".25" = load i64, i64* %"i"
  %".26" = load i32 addrspace(1)*, i32 addrspace(1)** %"b.1"
  %".27" = getelementptr i32, i32 addrspace(1)* %".26", i64 %".25"
  store i32 addrspace(1)* %".27", i32 addrspace(1)** %"tmp_clisp-pjtvcrce"
  %".29" = load i32 addrspace(1)*, i32 addrspace(1)** %"tmp_clisp-pjtvcrce"
  %".30" = load i32, i32 addrspace(1)* %".29", !invariant.load !0
  store i32 %".30", i32* %"tmp_clisp-msltedyy"
  %".32" = load i32, i32* %"tmp_clisp-kqfhnhbb"
  %".33" = load i32, i32* %"tmp_clisp-msltedyy"
  %"tmp_clisp-dofzglnn.1" = add i32 %".32", %".33"
  store i32 %"tmp_clisp-dofzglnn.1", i32* %"tmp_clisp-dofzglnn"
  %".35" = load i32 addrspace(1)*, i32 addrspace(1)** %"tmp_clisp-egjrlyhk"
  %".36" = load i32, i32* %"tmp_clisp-dofzglnn"
  store i32 %".36", i32 addrspace(1)* %".35"
  br label %"tmp_clisp.ret_lbl-piuzfbqp"
tmp_clisp.ret_lbl-piuzfbqp:
  ret void
}

!0 = !{  }
attributes #0 = { convergent mustprogress norecurse nounwind "denormal-fp-math"="preserve-sign,preserve-sign" "frame-pointer"="all" "no-signed-zeros-fp-math"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "sycl-entry-point" "sycl-module-id"="vector-add-usm.cpp" "sycl-optlevel"="2" "uniform-work-group-size"="true" }
attributes #1 = { convergent mustprogress nofree nounwind willreturn memory(none) "denormal-fp-math"="preserve-sign,preserve-sign" "frame-pointer"="all" "no-signed-zeros-fp-math"="true" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }


!100 = !{i32 1, i32 2} !101 = !{i32 4, i32 100000} 
!102 = !{!"Your compiler"} 
!103 = !{i32 1, !"wchar_size", i32 4} 
!104 = !{i32 1, !"sycl-device", i32 1} 
!105 = !{i32 7, !"frame-pointer", i32 2} 


!opencl.spir.version = !{!100} 
!spirv.Source = !{!101} 
!llvm.ident = !{!102} 
!llvm.module.flags = !{!103, !104, !105} 
!sycl.specialization-constants = !{} 
!sycl.specialization-constants-default-values = !{} 
!sycl-esimd-split-status = !{!106} 
!106 = !{i8 0}
