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

    private String groupName;

    private boolean enable;

    private Set<ExtraFileConfig> extraFileConfigNames;

    @Getter
    @Setter
    public static class ExtraFileConfig {
        private String name;

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
