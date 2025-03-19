namespace Kumir.NET.Runtime;

/// <summary>
/// Add exceptions to kumir
/// </summary>
public static class Exceptions
{
    public static void Fail(string message) => throw new Exception(message);
}