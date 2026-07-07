(c-lisp                                                                                                                                                                                                                               
     (define ((_Z33__spirv_BuiltInGlobalInvocationIdi int64) (dim int)) )  
                                                                                                                                                                                                                                                                                                                                                                                                                                                  
     (define ((kernel void)                                                                                                                                                                                                              
               (sum (ptr int (addrspace 1)))                                                                                                                                                                                                
               (a (ptr int (addrspace 1)))                                                                                                                                                                                                
               (b (ptr int (addrspace 1)))                                                                                                                                                                                              )
                                                                                                                                                                                                                          
          (declare i int64)                                                                                                                                                                                                                   
          (set i (call _Z33__spirv_BuiltInGlobalInvocationIdi 0)) 
                                                                                                                                                                                          
          (store (ptradd sum i)                                                                                                                                                                                                           
                (add (load (ptradd a i))                                                                                                                                                                                                 
                 (load (ptradd b i))))))
                   
 