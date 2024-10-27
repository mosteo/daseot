with Daseot;

procedure Test is

   package Trees is new Daseot (String);

   Tree : Trees.Tree;

begin
   Tree.Root.Set ("hello");
   pragma Assert (Tree.Root.Atom = "hello");
end Test;
