namespace XCarpaccio
{
  public struct Feedback
  {
      public string Type { get; set; }
      public string Content { get; set; }

      public override string ToString()
      {
        return $"[{Type}] {Content}";
      }
  }
}
