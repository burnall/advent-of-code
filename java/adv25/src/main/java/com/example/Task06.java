package com.example;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Task06 {
    private record Data(List<String> rows, List<Operation> ops) {
    }

    private enum Operation {
        MULTIPLY,
        ADD,
    }

    public static void main(String[] args) throws Exception {
        List<String> lines = Files.readAllLines(Paths.get("src/main/resources/t06.txt"));
        Data data = parseData(lines);
        System.out.println(part1(data));
        System.out.println(part2(data));
    }

    private static Data parseData(List<String> lines) {
        String lastLine = lines.removeLast();
        List<Operation> ops = Arrays.asList(lastLine.split("\\s+")).stream()
                .map(s -> s.equals("+") ? Operation.ADD : Operation.MULTIPLY)
                .toList();
        return new Data(lines, ops);
    }

    private static long part1(Data data) {
        var groups = readHorizontally(data.rows());
        return IntStream.range(0, data.ops().size())
                .mapToLong(i -> apply(groups.get(i), data.ops().get(i)))
                .sum();
    }

    private static long part2(Data data) {
        var groups = readVertically(data.rows());
        return IntStream.range(0, data.ops().size())
                .mapToLong(i -> apply(groups.get(i), data.ops().get(i)))
                .sum();
    }

    private static List<List<Long>> readHorizontally(List<String> rows) {
        List<List<Long>> numberRows = rows.stream()
                .map(s -> Arrays.stream(s.trim().split("\\s+"))
                        .map(Long::parseLong)
                        .toList())
                .toList();
        return IntStream.range(0, numberRows.get(0).size())
                .mapToObj(i -> numberRows.stream()
                        .map(row -> row.get(i))
                        .toList())
                .toList();
    }

    private static List<List<Long>> readVertically(List<String> rows) {
        List<String> cols = IntStream.range(0, rows.get(0).length())
                .mapToObj(i -> rows.stream()
                        .map(s -> i < s.length() ? Character.toString(s.charAt(i)) : "")
                        .collect(Collectors.joining()))
                .map(String::trim)
                .toList();

        List<List<Long>> groups = new ArrayList<>();
        List<Long>  current = new ArrayList<>();
        for (String col : cols) {
            if (col.isEmpty()) {
                if (!current.isEmpty()) {
                    groups.add(current);
                    current = new ArrayList<>();
                }
            } else {
                current.add(Long.parseLong(col));
            }
        }

        if (!current.isEmpty()) {
            groups.add(current);
        }
        return groups;
    }

    private static long apply(List<Long> values, Operation op) {
        return values.stream()
                .reduce((acc, v) -> op == Operation.ADD ? acc + v : acc * v)
                .orElse(0L);
    }
}
