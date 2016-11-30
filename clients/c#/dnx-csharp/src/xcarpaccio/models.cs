namespace XCarpaccio
{
  public class Feedback
  {
      public string Type { get; set; }
      public string Content { get; set; }

      public override string ToString()
      {
        return $"[{Type}] {Content}";
      }
  }
}
