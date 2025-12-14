package com.example;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Task05 {
    private record Range(long start, long end) {
    }

    private record Data(List<Range> ranges, List<Long> values) {
    }

    public static void main(String[] args) throws Exception {
        List<String> lines = Files.readAllLines(Paths.get("src/main/resources/t05.txt"));
        Data data = parse(lines);
//        System.out.println(part1(data));
        System.out.println(part2(data.ranges));
    }

    private static long part1(Data data) {
        return data.values.stream()
                .filter(v -> data.ranges.stream()
                        .anyMatch(range -> v >= range.start() && v <= range.end()))
                .count();
    }

    private static long part2(List<Range> ranges) {
        return trim(ranges).stream()
                .mapToLong(r -> r.end() - r.start() + 1)
                .sum();
    }

    private static Set<Range> trim(List<Range> ranges) {
        Set<Range> trimmed = new HashSet<>();
        trimmed.add(ranges.get(0));

        for (int i = 1; i < ranges.size(); i++) {
            addRange(trimmed, ranges.get(i));
        }
        return trimmed;
    }

    private static void addRange(Set<Range> ranges, Range range) {
        Set<Range> intersections = getIntersected(ranges, range);
        if (intersections.isEmpty()) {
            ranges.add(range);
            return;
        }
        Range joined = join(intersections, range);
        ranges.removeAll(intersections);
        ranges.add(joined);
    }

    private static Set<Range> getIntersected(Collection<Range> ranges, Range range) {
        Set<Range> intersections = new HashSet<>();
        for (Range r : ranges) {
            boolean noOverlap = range.start() > r.end() || r.start() > range.end();
            if (!noOverlap) {
                intersections.add(r);
            }
        }
        return intersections;
    }

    private static Range join(Set<Range> ranges, Range range) {
        Range joined = range;
        for (Range r : ranges) {
            joined = new Range(
                    Long.min(r.start(), joined.start()),
                    Long.max(r.end(), joined.end()));
        }
        return joined;
    }

    private static Data parse(List<String> lines) throws Exception {
        List<List<String>> parts = split(lines, "");
        if (parts.size() != 2) {
            throw new Exception("Wrong input");
        }
        List<Range> ranges = parts.get(0).stream()
                .map(Task05::parseRange)
                .toList();
        List<Long> values = parts.get(1).stream()
                .map(Long::parseLong)
                .toList();

        return new Data(ranges, values);
    }

    private static <T> List<List<T>> split(List<T> items, T elem) {
        List<List<T>> result = new ArrayList<>();
        List<T> current = new ArrayList<>();
        for (T item : items) {
            if (item.equals(elem)) {
                result.add(current);
                current = new ArrayList<>();
                continue;
            }
            current.add(item);
        }
        result.add(current);
        return result;
    }

    private static Range parseRange(String line) {
        String[] parts = line.split("-");
        return new Range(Long.parseLong(parts[0]), Long.parseLong(parts[1]));
    }
}
