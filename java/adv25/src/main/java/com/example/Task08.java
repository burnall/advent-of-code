package com.example;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

public class Task08 {
    private record Point(long x, long y, long z) {
    }

    private record Edge(Point start, Point end) {
    }

    public static void main(String[] args) throws Exception {
        List<String> lines = Files.readAllLines(Paths.get("src/main/resources/t08.txt"));
        var points = lines.stream()
                .map(Task08::parsePoint)
                .toList();
        System.out.println(part1(points));
    }

    private static Point parsePoint(String point) {
        var numbers = Arrays.stream(point.split(","))
                .map(Long::parseLong)
                .toList();
        return new Point(numbers.get(0), numbers.get(1), numbers.get(2));
    }

    private static double getDistance(Point a, Point b) {
        return Math.sqrt(Math.pow(b.x - a.x, 2) + Math.pow(b.y - a.y, 2) + Math.pow(b.z - a.z, 2));
    }

    private static List<Edge> getEdgesByDistance(List<Point> points) {
        SortedMap<Double, List<Edge>> distances = new TreeMap<>();
        for (int i = 0; i < points.size(); i++) {
            for (int j = i + 1; j < points.size(); j++) {
                Point a = points.get(i);
                Point b = points.get(j);
                Double d = getDistance(a, b);
                distances.compute(d, (k, v) -> {
                    List<Edge> bucket = v == null ? new ArrayList<>() : v;
                    bucket.add(new Edge(a, b));
                    return bucket;
                });
            }
        }
        return distances.values()
                .stream()
                .flatMap(Collection::stream)
                .toList();
    }

    private static void nextCircuits(Map<Point, Set<Point>> circuits, Edge edge) {
        Set<Point> circuit = circuits.get(edge.start);
        Set<Point> circuitEnd = circuits.get(edge.end);
        if (circuit == circuitEnd) {
            return;
        }
        circuit.addAll(circuitEnd);
        circuitEnd.stream().forEach(point -> circuits.put(point, circuit));
    }

    private static Map<Point, Set<Point>> getInitialCircuits(List<Point> points) {
        return points.stream()
                .map(point -> Map.entry(point, new HashSet<>(Set.of(point))))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    private static long part1(List<Point> points) {
        List<Edge> edges = getEdgesByDistance(points);
        Map<Point, Set<Point>> circuits = getInitialCircuits(points);
        for (int i = 0; i < 1000; i++) {
            nextCircuits(circuits, edges.get(i));
        }
        return circuits.values()
                .stream()
                .map(Set::size)
                .distinct()
                .sorted(Comparator.reverseOrder())
                .limit(3)
                .mapToLong(Integer::longValue)
                .reduce(1L,(a, b) -> a * b);
    }

    private static long part2(List<Point> points) {
        List<Edge> edges = getEdgesByDistance(points);
        Map<Point, Set<Point>> circuits = getInitialCircuits(points);
        Edge edge = null;
        for (Edge e : edges) {
            nextCircuits(circuits, e);
            if (circuits.get(e.start).size() == points.size()) {
                edge = e;
                break;
            }
        }
        if  (edge == null) {
            throw new RuntimeException("This is weird");
        }
        return edge.start().x * edge.end().x;
    }
}
