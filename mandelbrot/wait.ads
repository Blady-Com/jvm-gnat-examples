package Wait is
   procedure Wait_User_Thread_Termination;
end Wait;
pragma Import (Java, Wait, "wait");
