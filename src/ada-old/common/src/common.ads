with Common_Seizure_Detector_Config; use Common_Seizure_Detector_Config;

package Common with Pure, SPARK_Mode => On is

   Stride_Size : constant := Samples_Per_Stride;
   Epoch_Size  : constant := Stride_Size * Strides_Per_Epoch;

end Common;
