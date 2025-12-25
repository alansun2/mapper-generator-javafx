package com.alan344.utils;


import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * @author AlanSun
 */
public class StringUtils {

    /**
     * 字符串是否为空
     *
     * @param string 待校验字符串数组
     * @return true: all item is empty
     */
    public static boolean isEmpty(String... string) {
        for (String str : string) {
            if (str == null || "".equals(str.trim()) || "null".equals(str)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 字符串是否为空
     */
    public static boolean isEmpty(String str) {
        return str == null || "".equals(str.trim()) || "null".equals(str);
    }

    /**
     * 字符串是否不为空
     */
    public static boolean isNotEmpty(String str) {
        return !(str == null || "".equals(str.trim()) || "null".equals(str));
    }

    /**
     * <p>Checks if all of the CharSequences are empty ("") or null.</p>
     *
     * <pre>
     * StringUtils.isAllEmpty(null)             = true
     * StringUtils.isAllEmpty(null, "")         = true
     * StringUtils.isAllEmpty(new String[] {})  = true
     * StringUtils.isAllEmpty(null, "foo")      = false
     * StringUtils.isAllEmpty("", "bar")        = false
     * StringUtils.isAllEmpty("bob", "")        = false
     * StringUtils.isAllEmpty("  bob  ", null)  = false
     * StringUtils.isAllEmpty(" ", "bar")       = false
     * StringUtils.isAllEmpty("foo", "bar")     = false
     * </pre>
     *
     * @param css the CharSequences to check, may be null or empty
     * @return {@code true} if all of the CharSequences are empty or null
     * @since 3.6
     */
    public static boolean isAllEmpty(final String... css) {
        if (css == null || css.length == 0) {
            return true;
        }
        for (final String cs : css) {
            if (isNotEmpty(cs)) {
                return false;
            }
        }
        return true;
    }

    /**
     * 获取 trim 后字符串
     *
     * @param original original str
     * @return get after trim
     */
    public static String trim(String original) {
        return isNotEmpty(original) ? original.trim() : null;
    }

    /**
     * 取的UUID生成的随机数
     *
     * @return UUID
     */
    public static String getUUID() {
        String uuid = UUID.randomUUID().toString();
        return uuid.substring(0, uuid.indexOf("-"));
    }

    public static boolean hasLength(CharSequence str) {
        return (str != null && str.length() > 0);
    }

    public static boolean hasLength(String str) {
        return hasLength((CharSequence) str);
    }

    public static String trimAllWhitespace(String str) {
        if (!hasLength(str)) {
            return str;
        }
        int len = str.length();
        StringBuilder sb = new StringBuilder(str.length());
        for (int i = 0; i < len; i++) {
            char c = str.charAt(i);
            if (!Character.isWhitespace(c)) {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    /**
     * 比较两个字符串的长度。1 ： str1 > str2 ; 0: str1 = str2; -1 : str1 < str2
     *
     * @param str1 1
     * @param str2 2
     * @return 1 ： str1 > str2 ; 0: str1 = str2; -1 : str1 < str2
     */
    public static int compareStringLength(String str1, String str2) {
        int c = str1.compareTo(str2);
        return Integer.compare(c, 0);
    }

    /**
     * 判断 str 中是否存在被 regex 匹配的字符串
     *
     * @param str   字符串
     * @param regex 正则表达式
     * @return true: 存在
     */
    public static boolean matchPattern(String str, String regex) {
        Pattern p = Pattern.compile(regex);
        Matcher matcher = p.matcher(str);
        return matcher.find();
    }

    /**
     * 查找 str 中所有匹配的字符串
     *
     * @param str   字符串
     * @param regex 正则表达式
     * @return 所有匹配的字符串
     */
    public static List<String> getRegexMatchStr(String regex, String str) {
        Pattern compile = Pattern.compile(regex);
        Matcher matcher = compile.matcher(str);
        List<String> regexStr = new ArrayList<>();
        while (matcher.find()) {
            regexStr.add(matcher.group(0));
        }
        return regexStr;
    }

    /**
     * 判断是否包含数组中的字符串，只要存在一个就可以
     *
     * @param source 待校验的字符串
     * @param subStr 字串列表
     * @return true 保存
     */
    public static boolean contains(String source, String... subStr) {
        if (source == null) {
            return false;
        }
        for (String url : subStr) {
            if (source.contains(url)) {
                return true;
            }
        }

        return false;
    }

    /**
     * 如果为空则获取默认值
     *
     * @param str        str
     * @param defaultVal 默认值
     * @return str
     */
    public static String getDefaultIfNull(String str, String defaultVal) {
        return isEmpty(str) ? defaultVal : str;
    }

    /**
     * 如果为空则获取默认值
     *
     * @param str        str
     * @param supplier   str
     * @param defaultVal 默认值
     * @return str
     */
    public static String getDefaultIfNull(String str, Supplier<String> supplier, String defaultVal) {
        return isEmpty(str) ? defaultVal : supplier.get();
    }

    /**
     * 按指定的字节数截取字符串（一个中文字符占3个字节，一个英文字符或数字占1个字节）
     *
     * @param sourceString 源字符串
     * @param cutBytes     要截取的字节数
     * @return 返回截取后String
     */
    public static String subStringSpecifyBytes(String sourceString, int cutBytes) {
        if (StringUtils.isEmpty(sourceString)) {
            return "";
        }

        int totalBytes = 0, strTotalBytes = sourceString.getBytes().length;
        if (strTotalBytes <= cutBytes) {
            return sourceString;
        }

        int lastIndex = 0, strLength = sourceString.length();
        int last = strLength - 1;
        for (int i = last; i >= 0; i--) {
            String s = Integer.toBinaryString(sourceString.charAt(i));
            if (s.length() > 8) {
                totalBytes += 3;
            } else {
                totalBytes += 1;
            }

            if (strTotalBytes - totalBytes <= cutBytes) {
                lastIndex = i;
                break;
            }
        }

        return sourceString.substring(0, lastIndex);
    }
}
