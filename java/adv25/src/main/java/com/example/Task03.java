package com.example;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;


public class Task03 {
    public static void main(String[] args) throws Exception {
        List<String> lines = Files.readAllLines(Paths.get("src/main/resources/t03.txt"));
        System.out.println(part2(lines));
    }

    private static long part1(List<String> lines) {
        return lines.stream()
                .map(Task03::fingMax2)
                .mapToLong(Long::parseLong)
                .sum();
    }

    private static long part2(List<String> lines) {
        return lines.stream()
                .map(s -> findMaxN(s, 12))
                .map(String::valueOf)
                .mapToLong(Long::parseLong)
                .sum();
    }

    public static String fingMax2(String s) {
        int len = s.length();
        char c1 = '\0';
        char c0 = '\0';
        for (int i = 0; i < len; i++) {
            char ch = s.charAt(i);
            if (ch > c1) {
                if (i + 1 < len) {
                    c1 = ch;
                    c0 = '\0';
                } else {
                    c0 = ch;
                }
            } else if (ch > c0) {
                c0 = ch;
            }
        }
        return "" + c1 + c0;
    }

    private static char[] findMaxN(String s, int n) {
        char[] v = new char[n];
        int len = s.length();
        for (int i = 0; i < len; i++) {
            char ch = s.charAt(i);
            boolean found = false;
            for  (int j = n - Integer.min(n, len - i); j < n; j++) {
                if (found) {
                    v[j] = '\0';
                } else if (ch > v[j] ) {
                    v[j] = ch;
                    found = true;
                }
            }
        }
        return v;
    }
}
