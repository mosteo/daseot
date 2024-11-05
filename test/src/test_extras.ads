with Daseot.Filesystems;

package Test_Extras with Preelaborate is

   function Image (S : String) return String is (S);

   package Trees is new Daseot (String); use Trees;
   package FSs is new Trees.Filesystems (Image);

end Test_Extras;
