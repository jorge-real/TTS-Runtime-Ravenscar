package TT_Mixed_Criticality is
   pragma Pure(TT_Mixed_Criticality);
   
   type No_Criticality_Levels is (None);
   
   type Dual_Criticality_Levels is (LO, HI);
   
   --   Domain	                      Domain - Specific Safety Levels
   --  -------------------------------------------------------------------------- 
   --  Automotive (ISO 26262)          QM     ASIL-A    ASIL-B/C  ASIL-D	-
   --  General (IEC-61508)             -      SIL-1     SIL-2     SIL-3     SIL-4
   --  Aviation (ED-12/DO-178/DO-254)  DAL-E  DAL-D     DAL-C     DAL-B     DAL-A
   --  Railway (CENELEC 50126/128/129) -      SIL-1     SIL-2     SIL-3     SIL-4
   
   type ISO_26262_Criticality_Levels is (QM, ASIL_A, ASIL_B, ASIL_C, ASIL_D);
   
   type IEC_61508_Criticality_Levels is (SIL_1, SIL_2, SIL_3, SIL_4);
   
   type DO_178_Criticality_Levels is (DAL_E, DAL_D, DAL_C, DAL_B, DAL_A);
   type DO_254_Criticality_Levels is new DO_178_Criticality_Levels;
   
   type CENELEC_50126 is (SIL_1, SIL_2, SIL_3, SIL_4);

end TT_Mixed_Criticality;
