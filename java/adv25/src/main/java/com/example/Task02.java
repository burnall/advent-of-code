package com.example;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.LongStream;

record Range(long min, long max) {
}

public class Task02 {
    public static void main(String[] args) throws Exception {
        List<Range> ranges = Arrays.stream(Files.readString(Paths.get("src/main/resources/t02.txt"))
                        .split(","))
                .map(Task02::parse)
                .toList();
        System.out.println(task(ranges, Task02::isInvalid2));
    }

    private static long task(List<Range> ranges, Predicate<char[]> check) {
        return ranges.stream()
                .flatMapToLong(r ->
                        LongStream
                                .rangeClosed(r.min(), r.max())
                                .filter(i -> check.test(String.valueOf(i).toCharArray())))
                .sum();
    }

    private static boolean isInvalid(char[] chars) {
        if (chars.length % 2 == 1) {
            return false;
        }
        for (int i = 0, mid = chars.length / 2; i < mid; i += 1) {
            if (chars[i] != chars[mid + i]) {
                return false;
            }
        }
        return true;
    }

    private static boolean isInvalid2(char[] chars) {
        int len = chars.length;
        outer: for (int i = 1; i <= len / 2; i += 1) {
            if (len % i > 0) {
                continue;
            }
            int partitionCount = len / i;
            for (int j = 0; j < i; j += 1) {
                for (int k = 1; k < partitionCount; k += 1) {
                    if (chars[j] != chars[i * k + j]) {
                        continue outer;
                    }
                }
            }
            return true;
        }
        return false;
    }

    private static Range parse(String raw) {
        String[] parts = raw.split("-");
        return new Range(Long.parseLong(parts[0]), Long.parseLong(parts[1]));
    }
}
