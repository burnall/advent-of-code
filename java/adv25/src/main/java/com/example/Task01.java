package com.example;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

record Cmd(boolean clockwise, int clicks) {
};

public class Task01 {
    public static void main(String[] args) throws Exception {
        List<String> lines = Files.readAllLines(Paths.get("src/main/resources/t01.txt"));
        List<Cmd> cmds = parseCmds(lines);
        System.out.println(solve1(cmds));
    }

    private static long solve1(List<Cmd> cmds) {
        AtomicInteger sum = new AtomicInteger(50);
        return cmds.stream().map(cmd -> cmd.clicks() * (cmd.clockwise() ? 1 : -1))
                .map(sum::addAndGet)
                .filter(i -> i % 100 == 0)
                .count();
    }

    private static List<Cmd> parseCmds(List<String> lines) {
        return lines.stream()
                .map(line -> new Cmd('R' == line.charAt(0),
                        Integer.parseInt(line.substring(1))))
                .toList();
    }
}
