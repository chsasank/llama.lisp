; ModuleID = 'runtime/matrix.c'
source_filename = "runtime/matrix.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @random_matrix(ptr noundef %0, i32 noundef %1, i32 noundef %2) #0 {
  %4 = alloca ptr, align 8
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  store ptr %0, ptr %4, align 8
  store i32 %1, ptr %5, align 4
  store i32 %2, ptr %6, align 4
  %9 = call i32 (ptr, ...) @time(ptr noundef null)
  call void @srand(i32 noundef %9) #5
  store i32 0, ptr %7, align 4
  br label %10

10:                                               ; preds = %38, %3
  %11 = load i32, ptr %7, align 4
  %12 = load i32, ptr %6, align 4
  %13 = icmp slt i32 %11, %12
  br i1 %13, label %14, label %41

14:                                               ; preds = %10
  store i32 0, ptr %8, align 4
  br label %15

15:                                               ; preds = %34, %14
  %16 = load i32, ptr %8, align 4
  %17 = load i32, ptr %5, align 4
  %18 = icmp slt i32 %16, %17
  br i1 %18, label %19, label %37

19:                                               ; preds = %15
  %20 = call i32 @rand() #5
  %21 = sitofp i32 %20 to float
  %22 = fdiv float %21, 0x41E0000000000000
  %23 = fpext float %22 to double
  %24 = call double @llvm.fmuladd.f64(double 3.000000e+00, double %23, double -1.000000e+00)
  %25 = fptrunc double %24 to float
  %26 = load ptr, ptr %4, align 8
  %27 = load i32, ptr %7, align 4
  %28 = load i32, ptr %5, align 4
  %29 = mul nsw i32 %27, %28
  %30 = load i32, ptr %8, align 4
  %31 = add nsw i32 %29, %30
  %32 = sext i32 %31 to i64
  %33 = getelementptr inbounds float, ptr %26, i64 %32
  store float %25, ptr %33, align 4
  br label %34

34:                                               ; preds = %19
  %35 = load i32, ptr %8, align 4
  %36 = add nsw i32 %35, 1
  store i32 %36, ptr %8, align 4
  br label %15, !llvm.loop !6

37:                                               ; preds = %15
  br label %38

38:                                               ; preds = %37
  %39 = load i32, ptr %7, align 4
  %40 = add nsw i32 %39, 1
  store i32 %40, ptr %7, align 4
  br label %10, !llvm.loop !8

41:                                               ; preds = %10
  ret i32 1
}

; Function Attrs: nounwind
declare void @srand(i32 noundef) #1

declare i32 @time(...) #2

; Function Attrs: nounwind
declare i32 @rand() #1

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.fmuladd.f64(double, double, double) #3

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @ref_mult(ptr noundef %0, ptr noundef %1, ptr noundef %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) #0 {
  %7 = alloca ptr, align 8
  %8 = alloca ptr, align 8
  %9 = alloca ptr, align 8
  %10 = alloca i32, align 4
  %11 = alloca i32, align 4
  %12 = alloca i32, align 4
  %13 = alloca i32, align 4
  %14 = alloca i32, align 4
  %15 = alloca float, align 4
  %16 = alloca i32, align 4
  store ptr %0, ptr %7, align 8
  store ptr %1, ptr %8, align 8
  store ptr %2, ptr %9, align 8
  store i32 %3, ptr %10, align 4
  store i32 %4, ptr %11, align 4
  store i32 %5, ptr %12, align 4
  store i32 0, ptr %13, align 4
  br label %17

17:                                               ; preds = %69, %6
  %18 = load i32, ptr %13, align 4
  %19 = load i32, ptr %10, align 4
  %20 = icmp slt i32 %18, %19
  br i1 %20, label %21, label %72

21:                                               ; preds = %17
  store i32 0, ptr %14, align 4
  br label %22

22:                                               ; preds = %65, %21
  %23 = load i32, ptr %14, align 4
  %24 = load i32, ptr %11, align 4
  %25 = icmp slt i32 %23, %24
  br i1 %25, label %26, label %68

26:                                               ; preds = %22
  store float 0.000000e+00, ptr %15, align 4
  store i32 0, ptr %16, align 4
  br label %27

27:                                               ; preds = %52, %26
  %28 = load i32, ptr %16, align 4
  %29 = load i32, ptr %12, align 4
  %30 = icmp slt i32 %28, %29
  br i1 %30, label %31, label %55

31:                                               ; preds = %27
  %32 = load ptr, ptr %7, align 8
  %33 = load i32, ptr %16, align 4
  %34 = load i32, ptr %10, align 4
  %35 = mul nsw i32 %33, %34
  %36 = load i32, ptr %13, align 4
  %37 = add nsw i32 %35, %36
  %38 = sext i32 %37 to i64
  %39 = getelementptr inbounds float, ptr %32, i64 %38
  %40 = load float, ptr %39, align 4
  %41 = load ptr, ptr %8, align 8
  %42 = load i32, ptr %14, align 4
  %43 = load i32, ptr %12, align 4
  %44 = mul nsw i32 %42, %43
  %45 = load i32, ptr %16, align 4
  %46 = add nsw i32 %44, %45
  %47 = sext i32 %46 to i64
  %48 = getelementptr inbounds float, ptr %41, i64 %47
  %49 = load float, ptr %48, align 4
  %50 = load float, ptr %15, align 4
  %51 = call float @llvm.fmuladd.f32(float %40, float %49, float %50)
  store float %51, ptr %15, align 4
  br label %52

52:                                               ; preds = %31
  %53 = load i32, ptr %16, align 4
  %54 = add nsw i32 %53, 1
  store i32 %54, ptr %16, align 4
  br label %27, !llvm.loop !9

55:                                               ; preds = %27
  %56 = load float, ptr %15, align 4
  %57 = load ptr, ptr %9, align 8
  %58 = load i32, ptr %14, align 4
  %59 = load i32, ptr %10, align 4
  %60 = mul nsw i32 %58, %59
  %61 = load i32, ptr %13, align 4
  %62 = add nsw i32 %60, %61
  %63 = sext i32 %62 to i64
  %64 = getelementptr inbounds float, ptr %57, i64 %63
  store float %56, ptr %64, align 4
  br label %65

65:                                               ; preds = %55
  %66 = load i32, ptr %14, align 4
  %67 = add nsw i32 %66, 1
  store i32 %67, ptr %14, align 4
  br label %22, !llvm.loop !10

68:                                               ; preds = %22
  br label %69

69:                                               ; preds = %68
  %70 = load i32, ptr %13, align 4
  %71 = add nsw i32 %70, 1
  store i32 %71, ptr %13, align 4
  br label %17, !llvm.loop !11

72:                                               ; preds = %17
  ret i32 1
}

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare float @llvm.fmuladd.f32(float, float, float) #3

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i1 @compare_matrix(ptr noundef %0, ptr noundef %1, i32 noundef %2, i32 noundef %3) #0 {
  %5 = alloca i1, align 1
  %6 = alloca ptr, align 8
  %7 = alloca ptr, align 8
  %8 = alloca i32, align 4
  %9 = alloca i32, align 4
  %10 = alloca i32, align 4
  %11 = alloca i32, align 4
  store ptr %0, ptr %6, align 8
  store ptr %1, ptr %7, align 8
  store i32 %2, ptr %8, align 4
  store i32 %3, ptr %9, align 4
  store i32 0, ptr %10, align 4
  br label %12

12:                                               ; preds = %47, %4
  %13 = load i32, ptr %10, align 4
  %14 = load i32, ptr %9, align 4
  %15 = icmp slt i32 %13, %14
  br i1 %15, label %16, label %50

16:                                               ; preds = %12
  store i32 0, ptr %11, align 4
  br label %17

17:                                               ; preds = %43, %16
  %18 = load i32, ptr %11, align 4
  %19 = load i32, ptr %8, align 4
  %20 = icmp slt i32 %18, %19
  br i1 %20, label %21, label %46

21:                                               ; preds = %17
  %22 = load ptr, ptr %6, align 8
  %23 = load i32, ptr %10, align 4
  %24 = load i32, ptr %8, align 4
  %25 = mul nsw i32 %23, %24
  %26 = load i32, ptr %11, align 4
  %27 = add nsw i32 %25, %26
  %28 = sext i32 %27 to i64
  %29 = getelementptr inbounds float, ptr %22, i64 %28
  %30 = load float, ptr %29, align 4
  %31 = load ptr, ptr %7, align 8
  %32 = load i32, ptr %10, align 4
  %33 = load i32, ptr %8, align 4
  %34 = mul nsw i32 %32, %33
  %35 = load i32, ptr %11, align 4
  %36 = add nsw i32 %34, %35
  %37 = sext i32 %36 to i64
  %38 = getelementptr inbounds float, ptr %31, i64 %37
  %39 = load float, ptr %38, align 4
  %40 = fcmp une float %30, %39
  br i1 %40, label %41, label %42

41:                                               ; preds = %21
  store i1 false, ptr %5, align 1
  br label %51

42:                                               ; preds = %21
  br label %43

43:                                               ; preds = %42
  %44 = load i32, ptr %11, align 4
  %45 = add nsw i32 %44, 1
  store i32 %45, ptr %11, align 4
  br label %17, !llvm.loop !12

46:                                               ; preds = %17
  br label %47

47:                                               ; preds = %46
  %48 = load i32, ptr %10, align 4
  %49 = add nsw i32 %48, 1
  store i32 %49, ptr %10, align 4
  br label %12, !llvm.loop !13

50:                                               ; preds = %12
  store i1 true, ptr %5, align 1
  br label %51

51:                                               ; preds = %50, %41
  %52 = load i1, ptr %5, align 1
  ret i1 %52
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i1 @all_close(ptr noundef %0, ptr noundef %1, i32 noundef %2, i32 noundef %3, float noundef %4, float noundef %5, i1 noundef zeroext %6) #0 {
  %8 = alloca ptr, align 8
  %9 = alloca ptr, align 8
  %10 = alloca i32, align 4
  %11 = alloca i32, align 4
  %12 = alloca float, align 4
  %13 = alloca float, align 4
  %14 = alloca i8, align 1
  %15 = alloca float, align 4
  %16 = alloca float, align 4
  %17 = alloca ptr, align 8
  %18 = alloca i8, align 1
  %19 = alloca i32, align 4
  %20 = alloca i32, align 4
  store ptr %0, ptr %8, align 8
  store ptr %1, ptr %9, align 8
  store i32 %2, ptr %10, align 4
  store i32 %3, ptr %11, align 4
  store float %4, ptr %12, align 4
  store float %5, ptr %13, align 4
  %21 = zext i1 %6 to i8
  store i8 %21, ptr %14, align 1
  %22 = load i32, ptr %10, align 4
  %23 = load i32, ptr %11, align 4
  %24 = mul nsw i32 %22, %23
  %25 = sext i32 %24 to i64
  %26 = mul i64 %25, 1
  %27 = call noalias ptr @malloc(i64 noundef %26) #6
  store ptr %27, ptr %17, align 8
  store i32 0, ptr %19, align 4
  br label %28

28:                                               ; preds = %106, %7
  %29 = load i32, ptr %19, align 4
  %30 = load i32, ptr %11, align 4
  %31 = load i32, ptr %10, align 4
  %32 = mul nsw i32 %30, %31
  %33 = icmp slt i32 %29, %32
  br i1 %33, label %34, label %109

34:                                               ; preds = %28
  %35 = load ptr, ptr %8, align 8
  %36 = load i32, ptr %19, align 4
  %37 = sext i32 %36 to i64
  %38 = getelementptr inbounds float, ptr %35, i64 %37
  %39 = load float, ptr %38, align 4
  %40 = load ptr, ptr %9, align 8
  %41 = load i32, ptr %19, align 4
  %42 = sext i32 %41 to i64
  %43 = getelementptr inbounds float, ptr %40, i64 %42
  %44 = load float, ptr %43, align 4
  %45 = fsub float %39, %44
  %46 = call float @llvm.fabs.f32(float %45)
  store float %46, ptr %16, align 4
  %47 = load float, ptr %13, align 4
  %48 = load float, ptr %12, align 4
  %49 = load ptr, ptr %9, align 8
  %50 = load i32, ptr %19, align 4
  %51 = sext i32 %50 to i64
  %52 = getelementptr inbounds float, ptr %49, i64 %51
  %53 = load float, ptr %52, align 4
  %54 = call float @llvm.fabs.f32(float %53)
  %55 = call float @llvm.fmuladd.f32(float %48, float %54, float %47)
  store float %55, ptr %15, align 4
  %56 = load float, ptr %16, align 4
  %57 = load float, ptr %15, align 4
  %58 = fcmp ole float %56, %57
  %59 = zext i1 %58 to i8
  store i8 %59, ptr %18, align 1
  %60 = load i8, ptr %18, align 1
  %61 = trunc i8 %60 to i1
  %62 = zext i1 %61 to i32
  %63 = load ptr, ptr %9, align 8
  %64 = load i32, ptr %19, align 4
  %65 = sext i32 %64 to i64
  %66 = getelementptr inbounds float, ptr %63, i64 %65
  %67 = load float, ptr %66, align 4
  %68 = call float @llvm.fabs.f32(float %67) #7
  %69 = fcmp one float %68, 0x7FF0000000000000
  %70 = zext i1 %69 to i32
  %71 = and i32 %62, %70
  %72 = icmp ne i32 %71, 0
  %73 = zext i1 %72 to i8
  store i8 %73, ptr %18, align 1
  %74 = load i8, ptr %14, align 1
  %75 = trunc i8 %74 to i1
  br i1 %75, label %76, label %98

76:                                               ; preds = %34
  %77 = load ptr, ptr %8, align 8
  %78 = load i32, ptr %19, align 4
  %79 = sext i32 %78 to i64
  %80 = getelementptr inbounds float, ptr %77, i64 %79
  %81 = load float, ptr %80, align 4
  %82 = fcmp uno float %81, %81
  %83 = zext i1 %82 to i32
  %84 = load ptr, ptr %9, align 8
  %85 = load i32, ptr %19, align 4
  %86 = sext i32 %85 to i64
  %87 = getelementptr inbounds float, ptr %84, i64 %86
  %88 = load float, ptr %87, align 4
  %89 = fcmp uno float %88, %88
  %90 = zext i1 %89 to i32
  %91 = and i32 %83, %90
  %92 = load i8, ptr %18, align 1
  %93 = trunc i8 %92 to i1
  %94 = zext i1 %93 to i32
  %95 = or i32 %94, %91
  %96 = icmp ne i32 %95, 0
  %97 = zext i1 %96 to i8
  store i8 %97, ptr %18, align 1
  br label %98

98:                                               ; preds = %76, %34
  %99 = load i8, ptr %18, align 1
  %100 = trunc i8 %99 to i1
  %101 = load ptr, ptr %17, align 8
  %102 = load i32, ptr %19, align 4
  %103 = sext i32 %102 to i64
  %104 = getelementptr inbounds i8, ptr %101, i64 %103
  %105 = zext i1 %100 to i8
  store i8 %105, ptr %104, align 1
  br label %106

106:                                              ; preds = %98
  %107 = load i32, ptr %19, align 4
  %108 = add nsw i32 %107, 1
  store i32 %108, ptr %19, align 4
  br label %28, !llvm.loop !14

109:                                              ; preds = %28
  store i8 1, ptr %18, align 1
  store i32 0, ptr %20, align 4
  br label %110

110:                                              ; preds = %130, %109
  %111 = load i32, ptr %20, align 4
  %112 = load i32, ptr %10, align 4
  %113 = load i32, ptr %11, align 4
  %114 = mul nsw i32 %112, %113
  %115 = icmp slt i32 %111, %114
  br i1 %115, label %116, label %133

116:                                              ; preds = %110
  %117 = load ptr, ptr %17, align 8
  %118 = load i32, ptr %20, align 4
  %119 = sext i32 %118 to i64
  %120 = getelementptr inbounds i8, ptr %117, i64 %119
  %121 = load i8, ptr %120, align 1
  %122 = trunc i8 %121 to i1
  %123 = zext i1 %122 to i32
  %124 = load i8, ptr %18, align 1
  %125 = trunc i8 %124 to i1
  %126 = zext i1 %125 to i32
  %127 = and i32 %126, %123
  %128 = icmp ne i32 %127, 0
  %129 = zext i1 %128 to i8
  store i8 %129, ptr %18, align 1
  br label %130

130:                                              ; preds = %116
  %131 = load i32, ptr %20, align 4
  %132 = add nsw i32 %131, 1
  store i32 %132, ptr %20, align 4
  br label %110, !llvm.loop !15

133:                                              ; preds = %110
  %134 = load i8, ptr %18, align 1
  %135 = trunc i8 %134 to i1
  ret i1 %135
}

; Function Attrs: nounwind allocsize(0)
declare noalias ptr @malloc(i64 noundef) #4

; Function Attrs: nocallback nofree nosync nounwind readnone speculatable willreturn
declare float @llvm.fabs.f32(float) #3

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #4 = { nounwind allocsize(0) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #5 = { nounwind }
attributes #6 = { nounwind allocsize(0) }
attributes #7 = { readnone }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"clang version 15.0.5"}
!6 = distinct !{!6, !7}
!7 = !{!"llvm.loop.mustprogress"}
!8 = distinct !{!8, !7}
!9 = distinct !{!9, !7}
!10 = distinct !{!10, !7}
!11 = distinct !{!11, !7}
!12 = distinct !{!12, !7}
!13 = distinct !{!13, !7}
!14 = distinct !{!14, !7}
!15 = distinct !{!15, !7}
