package com.example;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Task07 {
    private record Point(int y, int x) {}

    public static void main(String[] args) throws Exception {
        List<String> map = Files.readAllLines(Paths.get("src/main/resources/t07.txt"));
        System.out.println(part2(map));
    }

    private static int part1(List<String> map) {
        Point start = findStart(map);
        AtomicInteger count = new AtomicInteger(0);
        Set<Point> points = Set.of(start);
        Runnable onSplit = count::incrementAndGet;
        while (!points.isEmpty()) {
            points = movePoints(points, map, onSplit);
        }
        return count.get();
    }

    private static long part2(List<String> map) {
        Point start = findStart(map);
        Map<Point, Long> timelineByPoint = Map.of(start, 1L);
        while (true) {
            var newSplits = movePoints2(timelineByPoint, map);
            // System.out.println(timelineByPoint);
            // System.out.println();
            if (newSplits.isEmpty()) {
                break;
            }
            timelineByPoint = newSplits;
        }
        return timelineByPoint.values().stream()
                .mapToLong(Long::longValue)
                .sum();
    }

    private static Point findStart(List<String> map) {
        for (int i = 0; i < map.size(); i++) {
            String line = map.get(i);
            for (int j = 0; j < line.length(); j++) {
                if (line.charAt(j) == 'S') {
                    return new Point(i, j);
                }
            }
        }
        throw new RuntimeException("S not found");
    }

    private static char get(List<String> map, Point point ) {
        return map.get(point.y).charAt(point.x);
    }

    private static Set<Point> movePoints(Set<Point> points, List<String> map, Runnable onSplit) {
        int maxY = map.size();
        int maxX = map.get(0).length();
        return points.stream().flatMap(point -> {
            if (point.y >= maxY) {
                return Stream.of();
            }
            if (get(map, point) != '^') {
                return Stream.of(new Point(point.y + 1, point.x));
            }
            onSplit.run();
            return Stream.of(new Point(point.y + 1, point.x - 1), new Point(point.y + 1, point.x + 1))
                    .filter(p -> p.x >= 0 && p.x < maxX);
        }).collect(Collectors.toSet());
    }

    private static Map<Point, Long> movePoints2(Map<Point, Long> timelineByPoint, List<String> map) {
        int maxY = map.size();
        int maxX = map.get(0).length();
        return timelineByPoint.entrySet().stream()
                .flatMap(entry -> {
            var point = entry.getKey();
            var cnt = entry.getValue();
            if (point.y >= maxY) {
                return Stream.of();
            }
            if (get(map, point) != '^') {
                return Stream.of(Map.entry(new Point(point.y + 1, point.x), cnt));
            }
            return Stream.of(new Point(point.y + 1, point.x - 1), new Point(point.y + 1, point.x + 1))
                    .filter(p -> p.x >= 0 && p.x < maxX)
                    .map(p -> Map.entry(p, cnt));
        }).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, Long::sum));
    }
}
