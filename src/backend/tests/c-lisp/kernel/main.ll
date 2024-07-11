; ModuleID = 'runtime/main.c'
source_filename = "runtime/main.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.str.1 = private unnamed_addr constant [6 x i8] c"%.2f\0A\00", align 1
@.str.3 = private unnamed_addr constant [6 x i8] c"%.5f \00", align 1
@.str.5 = private unnamed_addr constant [19 x i8] c"%.4fs %.4f Gflops\0A\00", align 1

; Function Attrs: nofree nounwind uwtable
define dso_local i32 @print(i32 noundef %0) local_unnamed_addr #0 {
  %2 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull @.str, i32 noundef %0)
  ret i32 134217727
}

; Function Attrs: nofree nounwind
declare noundef i32 @printf(ptr nocapture noundef readonly, ...) local_unnamed_addr #1

; Function Attrs: nofree nounwind uwtable
define dso_local i32 @fprint(float noundef %0) local_unnamed_addr #0 {
  %2 = fpext float %0 to double
  %3 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull @.str.1, double noundef %2)
  ret i32 2147483647
}

; Function Attrs: nofree nounwind uwtable
define dso_local i32 @print_char(i32 noundef %0) local_unnamed_addr #0 {
  %2 = tail call i32 @putchar(i32 %0)
  ret i32 2147483647
}

; Function Attrs: nofree nounwind uwtable
define dso_local i32 @print_matrix(ptr nocapture noundef readonly %0, i32 noundef %1, i32 noundef %2) local_unnamed_addr #0 {
  %4 = icmp sgt i32 %2, 0
  br i1 %4, label %5, label %14

5:                                                ; preds = %3
  %6 = icmp sgt i32 %1, 0
  %7 = sext i32 %1 to i64
  %8 = zext i32 %2 to i64
  %9 = zext i32 %1 to i64
  br label %10

10:                                               ; preds = %5, %16
  %11 = phi i64 [ 0, %5 ], [ %18, %16 ]
  br i1 %6, label %12, label %16

12:                                               ; preds = %10
  %13 = mul nsw i64 %11, %7
  br label %20

14:                                               ; preds = %16, %3
  %15 = tail call i32 @putchar(i32 10)
  ret i32 2147483647

16:                                               ; preds = %20, %10
  %17 = tail call i32 @putchar(i32 10)
  %18 = add nuw nsw i64 %11, 1
  %19 = icmp eq i64 %18, %8
  br i1 %19, label %14, label %10, !llvm.loop !5

20:                                               ; preds = %12, %20
  %21 = phi i64 [ 0, %12 ], [ %27, %20 ]
  %22 = add nsw i64 %21, %13
  %23 = getelementptr inbounds float, ptr %0, i64 %22
  %24 = load float, ptr %23, align 4, !tbaa !8
  %25 = fpext float %24 to double
  %26 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull @.str.3, double noundef %25)
  %27 = add nuw nsw i64 %21, 1
  %28 = icmp eq i64 %27, %9
  br i1 %28, label %16, label %20, !llvm.loop !12
}

; Function Attrs: nounwind uwtable
define dso_local i32 @random_matrix(ptr nocapture noundef writeonly %0, i32 noundef %1, i32 noundef %2) local_unnamed_addr #2 {
  %4 = tail call i64 @time(ptr noundef null) #11
  %5 = trunc i64 %4 to i32
  tail call void @srand(i32 noundef %5) #11
  %6 = icmp sgt i32 %2, 0
  br i1 %6, label %7, label %16

7:                                                ; preds = %3
  %8 = icmp sgt i32 %1, 0
  %9 = sext i32 %1 to i64
  %10 = zext i32 %2 to i64
  %11 = zext i32 %1 to i64
  br label %12

12:                                               ; preds = %7, %17
  %13 = phi i64 [ 0, %7 ], [ %18, %17 ]
  br i1 %8, label %14, label %17

14:                                               ; preds = %12
  %15 = mul nsw i64 %13, %9
  br label %20

16:                                               ; preds = %17, %3
  ret i32 1

17:                                               ; preds = %20, %12
  %18 = add nuw nsw i64 %13, 1
  %19 = icmp eq i64 %18, %10
  br i1 %19, label %16, label %12, !llvm.loop !13

20:                                               ; preds = %14, %20
  %21 = phi i64 [ 0, %14 ], [ %30, %20 ]
  %22 = tail call i32 @rand() #11
  %23 = sitofp i32 %22 to float
  %24 = fmul float %23, 0x3E00000000000000
  %25 = fpext float %24 to double
  %26 = tail call double @llvm.fmuladd.f64(double %25, double 3.000000e+00, double -1.000000e+00)
  %27 = fptrunc double %26 to float
  %28 = add nsw i64 %21, %15
  %29 = getelementptr inbounds float, ptr %0, i64 %28
  store float %27, ptr %29, align 4, !tbaa !8
  %30 = add nuw nsw i64 %21, 1
  %31 = icmp eq i64 %30, %11
  br i1 %31, label %17, label %20, !llvm.loop !14
}

; Function Attrs: nounwind
declare void @srand(i32 noundef) local_unnamed_addr #3

; Function Attrs: nounwind
declare i64 @time(ptr noundef) local_unnamed_addr #3

; Function Attrs: nounwind
declare i32 @rand() local_unnamed_addr #3

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare double @llvm.fmuladd.f64(double, double, double) #4

; Function Attrs: argmemonly nofree nosync nounwind uwtable
define dso_local i32 @ref_mult(ptr nocapture noundef readonly %0, ptr nocapture noundef readonly %1, ptr nocapture noundef writeonly %2, i32 noundef %3, i32 noundef %4, i32 noundef %5) local_unnamed_addr #5 {
  %7 = icmp sgt i32 %3, 0
  br i1 %7, label %8, label %19

8:                                                ; preds = %6
  %9 = icmp sgt i32 %4, 0
  %10 = icmp sgt i32 %5, 0
  %11 = sext i32 %3 to i64
  %12 = sext i32 %5 to i64
  %13 = sext i32 %3 to i64
  %14 = zext i32 %3 to i64
  %15 = zext i32 %4 to i64
  %16 = zext i32 %5 to i64
  br label %17

17:                                               ; preds = %8, %24
  %18 = phi i64 [ 0, %8 ], [ %25, %24 ]
  br i1 %9, label %20, label %24

19:                                               ; preds = %24, %6
  ret i32 1

20:                                               ; preds = %17, %27
  %21 = phi i64 [ %32, %27 ], [ 0, %17 ]
  br i1 %10, label %22, label %27

22:                                               ; preds = %20
  %23 = mul nsw i64 %21, %12
  br label %34

24:                                               ; preds = %27, %17
  %25 = add nuw nsw i64 %18, 1
  %26 = icmp eq i64 %25, %14
  br i1 %26, label %19, label %17, !llvm.loop !15

27:                                               ; preds = %34, %20
  %28 = phi float [ 0.000000e+00, %20 ], [ %44, %34 ]
  %29 = mul nsw i64 %21, %13
  %30 = add nsw i64 %29, %18
  %31 = getelementptr inbounds float, ptr %2, i64 %30
  store float %28, ptr %31, align 4, !tbaa !8
  %32 = add nuw nsw i64 %21, 1
  %33 = icmp eq i64 %32, %15
  br i1 %33, label %24, label %20, !llvm.loop !16

34:                                               ; preds = %22, %34
  %35 = phi i64 [ 0, %22 ], [ %45, %34 ]
  %36 = phi float [ 0.000000e+00, %22 ], [ %44, %34 ]
  %37 = mul nsw i64 %35, %11
  %38 = add nsw i64 %37, %18
  %39 = getelementptr inbounds float, ptr %0, i64 %38
  %40 = load float, ptr %39, align 4, !tbaa !8
  %41 = add nsw i64 %35, %23
  %42 = getelementptr inbounds float, ptr %1, i64 %41
  %43 = load float, ptr %42, align 4, !tbaa !8
  %44 = tail call float @llvm.fmuladd.f32(float %40, float %43, float %36)
  %45 = add nuw nsw i64 %35, 1
  %46 = icmp eq i64 %45, %16
  br i1 %46, label %27, label %34, !llvm.loop !17
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare float @llvm.fmuladd.f32(float, float, float) #4

; Function Attrs: argmemonly nofree norecurse nosync nounwind readonly uwtable
define dso_local zeroext i1 @compare_matrix(ptr nocapture noundef readonly %0, ptr nocapture noundef readonly %1, i32 noundef %2, i32 noundef %3) local_unnamed_addr #6 {
  %5 = icmp sgt i32 %3, 0
  br i1 %5, label %6, label %41

6:                                                ; preds = %4
  %7 = icmp sgt i32 %2, 0
  %8 = sext i32 %2 to i64
  %9 = sext i32 %2 to i64
  %10 = zext i32 %3 to i64
  %11 = zext i32 %2 to i64
  br label %15

12:                                               ; preds = %38
  %13 = add nuw nsw i64 %16, 1
  %14 = icmp eq i64 %13, %10
  br i1 %14, label %41, label %15, !llvm.loop !18

15:                                               ; preds = %6, %12
  %16 = phi i64 [ 0, %6 ], [ %13, %12 ]
  %17 = mul nsw i64 %16, %9
  br i1 %7, label %18, label %38

18:                                               ; preds = %15
  %19 = getelementptr inbounds float, ptr %0, i64 %17
  %20 = load float, ptr %19, align 4, !tbaa !8
  %21 = getelementptr inbounds float, ptr %1, i64 %17
  %22 = load float, ptr %21, align 4, !tbaa !8
  %23 = fcmp une float %20, %22
  br i1 %23, label %38, label %24

24:                                               ; preds = %18, %28
  %25 = phi i64 [ %26, %28 ], [ 0, %18 ]
  %26 = add nuw nsw i64 %25, 1
  %27 = icmp eq i64 %26, %11
  br i1 %27, label %35, label %28, !llvm.loop !19

28:                                               ; preds = %24
  %29 = add nsw i64 %26, %17
  %30 = getelementptr inbounds float, ptr %0, i64 %29
  %31 = load float, ptr %30, align 4, !tbaa !8
  %32 = getelementptr inbounds float, ptr %1, i64 %29
  %33 = load float, ptr %32, align 4, !tbaa !8
  %34 = fcmp une float %31, %33
  br i1 %34, label %35, label %24, !llvm.loop !19

35:                                               ; preds = %24, %28
  %36 = phi i32 [ 5, %24 ], [ 1, %28 ]
  %37 = icmp slt i64 %26, %8
  br label %38

38:                                               ; preds = %35, %18, %15
  %39 = phi i1 [ %7, %15 ], [ %7, %18 ], [ %37, %35 ]
  %40 = phi i32 [ 5, %15 ], [ 1, %18 ], [ %36, %35 ]
  br i1 %39, label %41, label %12

41:                                               ; preds = %38, %12, %4
  %42 = phi i32 [ 2, %4 ], [ %40, %38 ], [ 2, %12 ]
  %43 = icmp eq i32 %42, 2
  ret i1 %43
}

; Function Attrs: nofree nounwind uwtable
define dso_local zeroext i1 @all_close(ptr nocapture noundef readonly %0, ptr nocapture noundef readonly %1, i32 noundef %2, i32 noundef %3, float noundef %4, float noundef %5, i1 noundef zeroext %6) local_unnamed_addr #0 {
  %8 = mul nsw i32 %3, %2
  %9 = sext i32 %8 to i64
  %10 = tail call noalias ptr @malloc(i64 noundef %9) #12
  %11 = icmp sgt i32 %8, 0
  br i1 %11, label %12, label %14

12:                                               ; preds = %7
  %13 = zext i32 %8 to i64
  br label %18

14:                                               ; preds = %37, %7
  %15 = icmp sgt i32 %8, 0
  br i1 %15, label %16, label %45

16:                                               ; preds = %14
  %17 = zext i32 %8 to i64
  br label %47

18:                                               ; preds = %12, %37
  %19 = phi i64 [ 0, %12 ], [ %41, %37 ]
  %20 = getelementptr inbounds float, ptr %0, i64 %19
  %21 = load float, ptr %20, align 4, !tbaa !8
  %22 = getelementptr inbounds float, ptr %1, i64 %19
  %23 = load float, ptr %22, align 4, !tbaa !8
  %24 = fsub float %21, %23
  %25 = tail call float @llvm.fabs.f32(float %24)
  %26 = tail call float @llvm.fabs.f32(float %23)
  %27 = tail call float @llvm.fmuladd.f32(float %4, float %26, float %5)
  %28 = fcmp ole float %25, %27
  %29 = tail call float @llvm.fabs.f32(float %23) #13
  %30 = fcmp one float %29, 0x7FF0000000000000
  %31 = and i1 %30, %28
  br i1 %6, label %32, label %37

32:                                               ; preds = %18
  %33 = fcmp uno float %21, 0.000000e+00
  %34 = fcmp uno float %23, 0.000000e+00
  %35 = and i1 %33, %34
  %36 = or i1 %35, %31
  br label %37

37:                                               ; preds = %32, %18
  %38 = phi i1 [ %36, %32 ], [ %31, %18 ]
  %39 = getelementptr inbounds i8, ptr %10, i64 %19
  %40 = zext i1 %38 to i8
  store i8 %40, ptr %39, align 1, !tbaa !20
  %41 = add nuw nsw i64 %19, 1
  %42 = icmp eq i64 %41, %13
  br i1 %42, label %14, label %18, !llvm.loop !22

43:                                               ; preds = %47
  %44 = icmp ne i8 %52, 0
  br label %45

45:                                               ; preds = %43, %14
  %46 = phi i1 [ true, %14 ], [ %44, %43 ]
  ret i1 %46

47:                                               ; preds = %16, %47
  %48 = phi i64 [ 0, %16 ], [ %53, %47 ]
  %49 = phi i8 [ 1, %16 ], [ %52, %47 ]
  %50 = getelementptr inbounds i8, ptr %10, i64 %48
  %51 = load i8, ptr %50, align 1, !tbaa !20, !range !23
  %52 = and i8 %51, %49
  %53 = add nuw nsw i64 %48, 1
  %54 = icmp eq i64 %53, %17
  br i1 %54, label %43, label %47, !llvm.loop !24
}

; Function Attrs: inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0)
declare noalias noundef ptr @malloc(i64 noundef) local_unnamed_addr #7

; Function Attrs: mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn
declare float @llvm.fabs.f32(float) #4

; Function Attrs: mustprogress nofree norecurse nosync nounwind readnone willreturn uwtable
define dso_local float @flops(i32 noundef %0, i32 noundef %1, i32 noundef %2, float noundef %3) local_unnamed_addr #8 {
  %5 = shl i32 %0, 1
  %6 = mul i32 %5, %1
  %7 = mul i32 %6, %2
  %8 = sitofp i32 %7 to float
  %9 = fdiv float %8, %3
  %10 = fdiv float %9, 1.000000e+09
  ret float %10
}

; Function Attrs: nounwind uwtable
define dso_local i64 @timer() local_unnamed_addr #2 {
  %1 = tail call i64 @clock() #11
  ret i64 %1
}

; Function Attrs: nounwind
declare i64 @clock() local_unnamed_addr #3

; Function Attrs: nounwind uwtable
define dso_local i32 @main() local_unnamed_addr #2 {
  %1 = tail call noalias dereferenceable_or_null(16000000) ptr @malloc(i64 noundef 16000000) #12
  %2 = tail call noalias dereferenceable_or_null(16000000) ptr @malloc(i64 noundef 16000000) #12
  %3 = tail call noalias dereferenceable_or_null(16000000) ptr @malloc(i64 noundef 16000000) #12
  %4 = tail call i64 @time(ptr noundef null) #11
  %5 = trunc i64 %4 to i32
  tail call void @srand(i32 noundef %5) #11
  br label %6

6:                                                ; preds = %9, %0
  %7 = phi i64 [ 0, %0 ], [ %10, %9 ]
  %8 = mul nuw nsw i64 %7, 2000
  br label %12

9:                                                ; preds = %12
  %10 = add nuw nsw i64 %7, 1
  %11 = icmp eq i64 %10, 2000
  br i1 %11, label %24, label %6, !llvm.loop !13

12:                                               ; preds = %12, %6
  %13 = phi i64 [ 0, %6 ], [ %22, %12 ]
  %14 = tail call i32 @rand() #11
  %15 = sitofp i32 %14 to float
  %16 = fmul float %15, 0x3E00000000000000
  %17 = fpext float %16 to double
  %18 = tail call double @llvm.fmuladd.f64(double %17, double 3.000000e+00, double -1.000000e+00)
  %19 = fptrunc double %18 to float
  %20 = add nuw nsw i64 %13, %8
  %21 = getelementptr inbounds float, ptr %1, i64 %20
  store float %19, ptr %21, align 4, !tbaa !8
  %22 = add nuw nsw i64 %13, 1
  %23 = icmp eq i64 %22, 2000
  br i1 %23, label %9, label %12, !llvm.loop !14

24:                                               ; preds = %9
  %25 = tail call i64 @time(ptr noundef null) #11
  %26 = trunc i64 %25 to i32
  tail call void @srand(i32 noundef %26) #11
  br label %27

27:                                               ; preds = %30, %24
  %28 = phi i64 [ 0, %24 ], [ %31, %30 ]
  %29 = mul nuw nsw i64 %28, 2000
  br label %33

30:                                               ; preds = %33
  %31 = add nuw nsw i64 %28, 1
  %32 = icmp eq i64 %31, 2000
  br i1 %32, label %45, label %27, !llvm.loop !13

33:                                               ; preds = %33, %27
  %34 = phi i64 [ 0, %27 ], [ %43, %33 ]
  %35 = tail call i32 @rand() #11
  %36 = sitofp i32 %35 to float
  %37 = fmul float %36, 0x3E00000000000000
  %38 = fpext float %37 to double
  %39 = tail call double @llvm.fmuladd.f64(double %38, double 3.000000e+00, double -1.000000e+00)
  %40 = fptrunc double %39 to float
  %41 = add nuw nsw i64 %34, %29
  %42 = getelementptr inbounds float, ptr %2, i64 %41
  store float %40, ptr %42, align 4, !tbaa !8
  %43 = add nuw nsw i64 %34, 1
  %44 = icmp eq i64 %43, 2000
  br i1 %44, label %30, label %33, !llvm.loop !14

45:                                               ; preds = %30
  %46 = tail call i64 @clock() #11
  %47 = tail call i32 (ptr, ptr, ptr, i32, i32, i32, ...) @__MMult1(ptr noundef %1, ptr noundef %2, ptr noundef %3, i32 noundef 2000, i32 noundef 2000, i32 noundef 2000) #11
  %48 = tail call i64 @clock() #11
  %49 = tail call i64 @clock() #11
  %50 = sub nsw i64 0, %49
  %51 = sitofp i64 %50 to float
  %52 = sub nsw i64 0, %46
  %53 = sitofp i64 %52 to float
  %54 = sitofp i64 %48 to float
  %55 = fadd float %53, %54
  %56 = fdiv float %55, 1.000000e+06
  %57 = fdiv float 0xC1D194D800000000, %56
  %58 = fdiv float %57, 1.000000e+09
  %59 = tail call i64 @clock() #11
  %60 = sitofp i64 %59 to float
  %61 = fadd float %51, %60
  %62 = fdiv float %61, 1.000000e+06
  %63 = fdiv float 0xC1D194D800000000, %62
  %64 = fdiv float %63, 1.000000e+09
  %65 = fpext float %56 to double
  %66 = fpext float %58 to double
  %67 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull @.str.5, double noundef %65, double noundef %66)
  %68 = fpext float %62 to double
  %69 = fpext float %64 to double
  %70 = tail call i32 (ptr, ...) @printf(ptr noundef nonnull @.str.5, double noundef %68, double noundef %69)
  ret i32 0
}

declare i32 @__MMult1(...) local_unnamed_addr #9

; Function Attrs: nofree nounwind
declare noundef i32 @putchar(i32 noundef) local_unnamed_addr #10

attributes #0 = { nofree nounwind uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nofree nounwind "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { nounwind uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { nounwind "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { mustprogress nocallback nofree nosync nounwind readnone speculatable willreturn }
attributes #5 = { argmemonly nofree nosync nounwind uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #6 = { argmemonly nofree norecurse nosync nounwind readonly uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #7 = { inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0) "alloc-family"="malloc" "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #8 = { mustprogress nofree norecurse nosync nounwind readnone willreturn uwtable "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #9 = { "frame-pointer"="none" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #10 = { nofree nounwind }
attributes #11 = { nounwind }
attributes #12 = { nounwind allocsize(0) }
attributes #13 = { readnone }

!llvm.module.flags = !{!0, !1, !2, !3}
!llvm.ident = !{!4}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{!"clang version 15.0.5"}
!5 = distinct !{!5, !6, !7}
!6 = !{!"llvm.loop.mustprogress"}
!7 = !{!"llvm.loop.unroll.disable"}
!8 = !{!9, !9, i64 0}
!9 = !{!"float", !10, i64 0}
!10 = !{!"omnipotent char", !11, i64 0}
!11 = !{!"Simple C/C++ TBAA"}
!12 = distinct !{!12, !6, !7}
!13 = distinct !{!13, !6, !7}
!14 = distinct !{!14, !6, !7}
!15 = distinct !{!15, !6, !7}
!16 = distinct !{!16, !6, !7}
!17 = distinct !{!17, !6, !7}
!18 = distinct !{!18, !6, !7}
!19 = distinct !{!19, !6, !7}
!20 = !{!21, !21, i64 0}
!21 = !{!"_Bool", !10, i64 0}
!22 = distinct !{!22, !6, !7}
!23 = !{i8 0, i8 2}
!24 = distinct !{!24, !6, !7}
