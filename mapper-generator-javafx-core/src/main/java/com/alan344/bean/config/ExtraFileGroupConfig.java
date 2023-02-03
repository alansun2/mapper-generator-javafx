package com.alan344.bean.config;

import lombok.Getter;
import lombok.Setter;

import java.util.Set;

/**
 * @author AlanSun
 * @date 2022/11/21 14:32
 */
@Getter
@Setter
public class ExtraFileGroupConfig {
    /**
     * 分组名称
     */
    private String groupName;
    /**
     * 是否开启
     */
    private boolean enable;
    /**
     * 额外文件的名称
     */
    private Set<ExtraFileConfig> extraFileConfigNames;

    @Getter
    @Setter
    public static class ExtraFileConfig {
        private String groupName;
        private String name;
        /**
         * 文件输出地址
         */
        private String outputPath;
        /**
         * 包名
         */
        private String packageName;
        /**
         * 是否开启
         */
        private boolean enable;

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            ExtraFileConfig that = (ExtraFileConfig) o;

            return name.equals(that.name);
        }

        @Override
        public int hashCode() {
            return name.hashCode();
        }
    }
}
