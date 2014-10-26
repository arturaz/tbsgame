package utils

/**
 * Created by arturas on 2014-10-26.
 */
object Base36 {
  private[this] val clist = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray
//    public static long Decode(string inputString) {
//      long result = 0;
//      var pow = 0;
//      for (var i = inputString.Length - 1; i >= 0; i--) {
//        var c = inputString[i];
//        var pos = Clist.IndexOf(c);
//        if (pos > -1)
//          result += pos * (long)Math.Pow(Clist.Length, pow);
//        else
//        return -1;
//        pow++;
//      }
//      return result;
//    }

  def encode(inputNumber: Long) = {
    val sb = new StringBuilder
    var num = inputNumber
    do {
      sb.append(clist((num % clist.length).abs.toInt))
      num /= clist.length
    } while (num != 0)
    sb.toString().reverse
  }
}
