package FFTW.Double.Formal is
   new FFTW.Formal (
   Float_Type => Long_Float,
   Plan_Type  => Plan_Type,
   Complex    => Complex,
   Create     => Create,
   Execute    => Execute);
