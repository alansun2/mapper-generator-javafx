package com.alan344.constants.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

/**
 * @author AlanSun
 * @date 2023/4/22 23:06
 * <p>
 * 文件写入方式
 */
@Getter
@AllArgsConstructor
public enum FileWriteModeEnum {
    /**
     * 合并
     */
    MERGE("合并"),
    /**
     * 覆盖
     */
    OVERWRITE("覆盖");

    private final String value;

    private static final Map<String, FileWriteModeEnum> ENUM_MAP = new HashMap<>();

    static {
        for (FileWriteModeEnum type : values()) {
            ENUM_MAP.put(type.getValue(), type);
        }
    }

    public static FileWriteModeEnum getEnum(String value) {
        return ENUM_MAP.get(value);
    }

    public static List<String> valuesToString() {
        return Stream.of(FileWriteModeEnum.values()).map(FileWriteModeEnum::getValue).toList();
    }
}
