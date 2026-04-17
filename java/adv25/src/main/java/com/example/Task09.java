package com.example;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class Task09 {
    private record Point(long x, long y) {}

    public static void main(String[] args) throws Exception {
        List<String> lines = Files.readAllLines(Paths.get("src/main/resources/t09.txt"));
        List<Point> points = lines.stream()
                .map(Task09::parsePoint)
                .toList();
        System.out.println(part2(points));
    }

    private static long part1(List<Point> points) {
        long max = 0;
        for (int i = 0; i <= points.size() - 1; i++) {
            for  (int j = i + 1; j <= points.size() - 1; j++) {
                long area = getArea(points.get(i), points.get(j));
                if (area > max) {
                    max = area;
                }
            }
        }
        return max;
    }

    private static Point parsePoint(String point) {
        var numbers = Arrays.stream(point.split(","))
                .map(Long::parseLong)
                .toList();
        return new Point(numbers.get(0), numbers.get(1));
    }

    private static long getArea(Point a, Point b) {
        return (Math.abs(a.x - b.x) + 1) * (Math.abs(a.y - b.y) + 1);
    }

    private static long part2(List<Point> points) {
        long max = 0;
        for (int i = 0; i < points.size() ; i++) {
            for  (int j = i + 1; j < points.size(); j++) {
                long area = getArea(points.get(i), points.get(j));
                if (area > max && isInside(points.get(i), points.get(j), points)) {
                    System.out.println(String.format("%s %s %d", points.get(i), points.get(j), area));
                    max = area;
                }
            }
        }
        return max;
    }

    private static boolean isInside(Point cornerA, Point cornerB, List<Point> points) {
        for (int i = 0; i < points.size(); i++) {
            var a = points.get(i);
            if (strictBetween(a.x, cornerA.x, cornerB.x) && strictBetween(a.y, cornerA.y, cornerB.y)) {
                return false;
            }
            var b = points.get((i + 1) % points.size());
            if (intersects(cornerA, cornerB, a, b)) {
                return false;
            }
        }

        return true;
    }

    private static boolean intersects(Point cornerA, Point cornerB, Point v, Point w) {
        boolean horizontal = v.y == w.y;
        if (horizontal) {
            return strictBetween(v.y, cornerA.y, cornerB.y) &&
                    between(cornerA.x, v.x, w.x) && between(cornerB.x, v.x, w.x);
        }
        return strictBetween(v.x, cornerA.x, cornerB.x) &&
                between(cornerA.y, v.y, w.y) && between(cornerB.y, v.y, w.y);
    }

    private static boolean strictBetween(long v, long a, long b) {
        return v > Math.min(a, b) && v < Math.max(a, b);
    }

    private static boolean between(long v, long a, long b) {
        return v >= Math.min(a, b) && v <= Math.max(a, b);
    }
}
