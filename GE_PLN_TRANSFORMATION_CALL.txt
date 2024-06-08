CREATE OR REPLACE PACKAGE BODY GE_PLN_TRANSFORMATION_CALL AS

  x_error                VARCHAR2(3000);
  enable_flag            VARCHAR2(100);
  spm_enable_flag        VARCHAR2(100);
  x_flow                 VARCHAR2(100);
  x_activity_type        VARCHAR2(100);
  num_of_record          NUMBER;
  num_of_records         NUMBER;
  num_of_program_running NUMBER;
  x_number               VARCHAR2(4000);
  p_start_date           DATE;
  p_sequence             VARCHAR2(100);
  x_message              VARCHAR2(4000) := '';
  p_status               VARCHAR2(20);
  v_debug_status         VARCHAR2(50);
  v_debug_message        VARCHAR2(4000) := '';
  p_rows                 NUMBER;
  V_MW_CALL              VARCHAR2(100):= 'N';
  x_status               VARCHAR2(20);
  v_seq                  NUMBER:= 0;
  
  PROCEDURE GE_PLN_TRANSFORMATION_FLOW(p_activity_name IN VARCHAR2,
                                       p_status        OUT VARCHAR2,
                                       p_message       OUT VARCHAR2) IS
  
    
    CURSOR total_data_stream(p_activity_name varchar2) IS
      select DATA_STREAM
        from GE_SPM_RULE_HEADERS_ALL
       where ACTIVITY_NAME = p_activity_name
         and enable_flag = 'Y'
       order by DATASTREAM_WEIGHTAGE;
  
   
    CURSOR total_spm_enable_stream(p_activity_name varchar2) IS
      select DATA_STREAM
        from GE_SPM_RULE_HEADERS_ALL
       where ACTIVITY_NAME = p_activity_name
         and SPM_ENABLE_FLAG = 'Y'
         and enable_flag = 'Y'
       order by DATASTREAM_WEIGHTAGE;
  
  BEGIN
  
    p_status     := 'S';
    p_start_date := SYSDATE;
    p_sequence   := IFACE_SPM_TABLE_S.NEXTVAL;
    x_message    := 'Package GE_PLN_TRANSFORMATION_CALL is called for  ' ||
                    p_activity_name;
  
    
    BEGIN
      SELECT VALUE
        INTO v_debug_status
        FROM GE_PLN_SYSTEM_CONTROLS
       WHERE CONTROL_TYPE = 'DEBUG_CONTROL'
         AND ACTIVITY = p_activity_name;
    EXCEPTION
      when others then
        v_debug_status := 'NO';
    END;
    
    IF (v_debug_status != 'YES') THEN
      v_debug_message := 'Debug mode is disabled for the ' ||
                         p_activity_name;
    ELSE
      v_debug_message := 'Debug mode is enabled for the ' ||
                         p_activity_name;
    END IF;
    
    BEGIN
    
      GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_INSERT('execute_transformation',
                                               'GE_PLN_TRANSFORMATION',
                                               SYSDATE,
                                               SYSDATE,
                                               x_message,
                                               v_debug_message,
                                               'I',
                                               p_activity_name,
                                               p_sequence);
    
    EXCEPTION
      when others then
       
        p_status  := 'E';
        p_message := 'Error in program_id ' || p_sequence ||
                     ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_INSERT can not be called ';
        x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_INSERT can not be called ' ||
                     sqlerrm;
        IF (v_debug_status = 'YES') THEN
          v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_INSERT can not be called';
        END IF;
      
    END;
  
    select count(*)
      into num_of_program_running
      from GEMS_IFACE_SPM_TABLE
     where ACTIVITY_NAME = p_activity_name
       and STATUS_FLAG not in ('E', 'C')
       and creation_date >= sysdate - 300 / 1440; --- (SYSDATE - 5 hours)
    v_debug_message := NULL;
    
    IF (num_of_program_running = 1) THEN
      

      IF (p_status != 'E') THEN
        
 ----------------------------- Before INBD STUB  START   --------------------------------
  
  GE_PLN_REQUEST_SET(p_activity_name,'BEFORE_IP',x_status,x_message);
  p_status := NVL(x_status,'S');
  ----------------------------- Before INBD STUB  END   --------------------------------
    
       
      
        x_message := 'STUB execution is started for INBD to PRSD flow for  ' ||
                     p_activity_name || ' ';
      
        IF (v_debug_status = 'YES') THEN
          v_debug_message := 'STUB execution is started for INBD to PRSD flow for  ';
        END IF;
         
        BEGIN
        
          GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE('GE_INBD_PRSD_FLOW',
                                                   'GE_INBD_PRSD_STUB',
                                                   SYSDATE,
                                                   x_message,
                                                   v_debug_message,
                                                   'I',
                                                   p_activity_name,
                                                   NULL,
                                                   NULL,
                                                   NULL,
                                                   p_sequence);
        
        EXCEPTION
          when others then
           
            p_status  := 'E';
            p_message := 'Error in program_id ' || p_sequence ||
                         ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ';
            x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ' ||
                         sqlerrm;
          
            IF (v_debug_status = 'YES') THEN
              v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ';
            END IF;
          
        END;
        IF (p_status != 'E') THEN
        
        
          BEGIN
          
            PDS.GE_INBD_PRSD_STUB.GE_INBD_PRSD_FLOW(p_activity_name,
                                                    p_status,
                                                    p_message); 
            
          
          EXCEPTION
            when others then
            
              p_status  := 'E';
              p_message := 'Error in program_id ' || p_sequence ||
                           ' GE_INBD_PRSD_STUB.GE_INBD_PRSD_FLOW can not be called ';
              x_message := 'GE_INBD_PRSD_STUB.GE_INBD_PRSD_FLOW can not be called ' ||
                           sqlerrm;
            
              IF (v_debug_status = 'YES') THEN
                v_debug_message := 'GE_INBD_PRSD_STUB.GE_INBD_PRSD_FLOW can not be called ';
              END IF;
          END;
        
          IF (p_status != 'E') THEN
          
            x_message := ' Ended at ' || p_activity_name ||
                         ' with status ' || p_message;
            --p_message := ' GE_INBD_PRSD_STUB Is Executed ';
          
            IF (v_debug_status = 'YES') THEN
              v_debug_message := 'GE_INBD_PRSD_STUB Is Executed with status ' ||
                                 p_message;
            END IF;
            BEGIN
            
              GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                           x_message,
                                                           v_debug_message,
                                                           'W',
                                                           p_sequence,
                                                           '0');
            EXCEPTION
              when others then
                
                p_status  := 'E';
                p_message := 'Error in program_id ' || p_sequence ||
                             ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ' ||
                             sqlerrm;
              
                IF (v_debug_status = 'YES') THEN
                  v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                END IF;
              
            END;
            IF (p_status != 'E') THEN
            
             
              select count(*)
                into num_of_record
                from GE_SPM_RULE_HEADERS_ALL
               where ACTIVITY_NAME = p_activity_name
                 and ENABLE_FLAG = 'Y';
            
              if (num_of_record > 0) then
              
                for individual_data_stream in total_data_stream(p_activity_name) loop
                  
                  x_flow          := 'IP';
                  x_activity_type := 'INSERT';
                
                  x_message := individual_data_stream.data_stream ||
                               ' for  ' || x_activity_type ||
                               ' activity  in ' || x_flow ||
                               ' flow Started at ';
                
                  IF (v_debug_status = 'YES') THEN
                    v_debug_message := individual_data_stream.data_stream ||
                                       ' for  ' || x_activity_type ||
                                       ' activity  in ' || x_flow ||
                                       ' flow Started  ';
                  END IF;
                
                 
                
                  BEGIN
                  
                    GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE('execute_transformation',
                                                             'GE_PLN_TRANSFORMATION',
                                                             SYSDATE,
                                                             x_message,
                                                             v_debug_message,
                                                             'I',
                                                             p_activity_name,
                                                             individual_data_stream.data_stream,
                                                             x_activity_type,
                                                             x_flow,
                                                             p_sequence);
                  
                  EXCEPTION
                    when others then
                      
                      p_status  := 'E';
                      p_message := 'Error in program_id ' || p_sequence ||
                                   ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ';
                      x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ' ||
                                   sqlerrm;
                      IF (v_debug_status = 'YES') THEN
                        v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ';
                      END IF;
                      EXIT;
                    
                  END;
                
               
                  BEGIN
                    GE_PLN_TRANSFORMATION.execute_transformation(p_activity_name => p_activity_name,
                                                                 p_data_stream   => individual_data_stream.data_stream,
                                                                 p_activity_type => x_activity_type,
                                                                 p_error         => x_error,
                                                                 p_flow          => x_flow,
                                                                 p_number        => x_number,
                                                                 p_rows          => p_rows);
                  
                    IF (x_error = 'SUCCESS') THEN
                      x_message := 'Status : ' || x_error ||',' ||
                                   ' Number of Rows Processed ' || x_number;
                      p_message := 'Completed IP flow Upto ' ||
                                   individual_data_stream.data_stream ||
                                   ' for ' || p_activity_name;
                      p_status  := 'W'; 
                    
                      IF (v_debug_status = 'YES') THEN
                        v_debug_message := 'Completed IP Insert flow and ' ||
                                           ' End with status ' || x_error ||
                                           ' of ' ||
                                           individual_data_stream.data_stream ||
                                           ' for' || p_activity_name ||
                                           ' Number of Rows Processed ' ||
                                           x_number;
                      END IF;
                    
                    ELSIF (x_error = 'WARNING') THEN
                      x_message := 'Status : ' || x_error ||',' ||
                                   ' No Querie Exist' 
                       ;
                      /*p_message := 'Completed IP flow Upto ' ||
                      individual_spm_data_stream.data_stream ||
                      ' for' || p_activity_name;*/
                      p_status := 'W'; 
                    
                      IF (v_debug_status = 'YES') THEN
                        v_debug_message := 'Completed IP INSERT flow. Status : ' ||
                                           x_error /*||
                                                                                 ' Number of Rows Processed ' ||
                                                                                 x_number*/
                                           || ' No Querie Exist';
                      END IF;
                    else
                      x_message := 'Status : ' || x_error;
                      p_status  := 'E'; 
                    
                      p_message := 'Error in program_id ' || p_sequence;
                      IF (v_debug_status = 'YES') THEN
                        v_debug_message := 'Error in IP flow Upto ' ||
                                           individual_data_stream.data_stream ||
                                           ' for' || p_activity_name;
                      END IF;
                    
                      EXIT;
                    end if;
                  
                  EXCEPTION
                    when others then
                     
                      p_status  := 'E';
                      p_message := 'Error in program_id ' || p_sequence ||
                                   ' GE_PLN_TRANSFORMATION.execute_transformation can not be called ';
                      x_message := 'GE_PLN_TRANSFORMATION.execute_transformation can not be called ' ||
                                   sqlerrm;
                    
                      IF (v_debug_status = 'YES') THEN
                        v_debug_message := 'EGE_PLN_TRANSFORMATION.execute_transformation can not be called';
                      
                      END IF;
                      EXIT;
                    
                  END;
                
                 
                
                  BEGIN
                  
                    GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                 x_message,
                                                                 v_debug_message,
                                                                 p_status,
                                                                 p_sequence,
                                                                 p_rows);
                  
                  EXCEPTION
                    when others then
                     
                      p_status  := 'E';
                      p_message := 'Error in program_id ' || p_sequence ||
                                   ' GE_PLN_TRANSFORMATION.execute_transformation can not be called ';
                      x_message := 'GE_PLN_TRANSFORMATION.execute_transformation can not be called ' ||
                                   sqlerrm;
                    
                      IF (v_debug_status = 'YES') THEN
                        v_debug_message := 'GE_PLN_TRANSFORMATION.execute_transformation can not be called ';
                      
                      END IF;
                      EXIT;
                    
                  END;
                
                  if (x_error = 'SUCCESS') THEN
                    
                    
                    x_activity_type := 'UPDATE';
                  
                    x_message := individual_data_stream.data_stream ||
                                 ' for  ' || x_activity_type ||
                                 ' activity  in ' || x_flow ||
                                 ' flow Started at ';
                  
                    IF (v_debug_status = 'YES') THEN
                      v_debug_message := individual_data_stream.data_stream ||
                                         ' for  ' || x_activity_type ||
                                         ' activity  in ' || x_flow ||
                                         ' flow Started ';
                    
                    END IF;
                    BEGIN
                    
                      GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE('execute_transformation',
                                                               'GE_PLN_TRANSFORMATION',
                                                               SYSDATE,
                                                               x_message,
                                                               v_debug_message,
                                                               'I',
                                                               p_activity_name,
                                                               individual_data_stream.data_stream,
                                                               x_activity_type,
                                                               x_flow,
                                                               p_sequence);
                    EXCEPTION
                      when others then
                        
                        p_status  := 'E';
                        p_message := 'Error in program_id ' || p_sequence ||
                                     ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ';
                        x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ' ||
                                     sqlerrm;
                      
                        IF (v_debug_status = 'YES') THEN
                          v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ';
                        
                        END IF;
                        EXIT;
                      
                    END;
                  
                    BEGIN
                      GE_PLN_TRANSFORMATION.execute_transformation(p_activity_name => p_activity_name,
                                                                   p_data_stream   => individual_data_stream.data_stream,
                                                                   p_activity_type => x_activity_type,
                                                                   p_error         => x_error,
                                                                   p_flow          => x_flow,
                                                                   p_number        => x_number,
                                                                   p_rows          => p_rows);
                    
                      if (x_error = 'SUCCESS') THEN
                        x_message := 'Status : ' || x_error ||',' ||
                                     ' Number of Rows Processed ' ||
                                     x_number;
                        p_message := 'Completed IP flow Upto ' ||
                                     individual_data_stream.data_stream ||
                                     ' for' || p_activity_name;
                        p_status  := 'W'; 
                        IF (v_debug_status = 'YES') THEN
                          v_debug_message := 'Completed IP Update flow ' ||
                                             ' Status : ' || x_error ||
                                             ' of ' ||
                                             individual_data_stream.data_stream ||
                                             ' for' || p_activity_name ||
                                             ' Number of Rows Processed ' ||
                                             x_number;
                        
                        END IF;
                      
                      ELSIF (x_error = 'WARNING') THEN
                        x_message := 'Status : ' || x_error ||',' ||
                                     ' No Querie Exist' /*||
                                                               ' Number of Rows Processed ' ||
                                                               x_number*/
                         ;
                        /* p_message := 'Completed IP flow Upto ' ||
                        individual_spm_data_stream.data_stream ||
                        ' for' || p_activity_name;*/
                        p_status := 'W';
                      
                        IF (v_debug_status = 'YES') THEN
                          v_debug_message := 'Completed IP INSERT flow.. End with status ' ||
                                             x_error /*||
                                                                                   ' Number of Rows Processed ' ||
                                                                                   x_number*/
                                             || ' No Querie Exist';
                        END IF;
                      else
                        x_message := ' Status : ' || x_error;
                        --  p_status  := 'W';
                        p_status := 'E'; 
                        IF (v_debug_status = 'YES') THEN
                          v_debug_message := 'Error in IP flow Upto ' ||
                                             individual_data_stream.data_stream ||
                                             ' for' || p_activity_name;
                        
                        END IF;
                      
                        p_message := 'Error in program_id ' || p_sequence;
                        EXIT; 
                      end if;
                    
                    exception
                      when others then
                       
                      
                        p_status  := 'E';
                        p_message := 'Error in program_id ' || p_sequence;
                        x_message := 'End with status ERROR - ' ||
                                     sqlerrm;
                        IF (v_debug_status = 'YES') THEN
                          v_debug_message := 'Execute_transformation can not be called';
                        
                        END IF;
                        EXIT;
                    end;
                  
                    BEGIN
                    
                      GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                   x_message,
                                                                   v_debug_message,
                                                                   p_status,
                                                                   p_sequence,
                                                                   p_rows);
                    
                    EXCEPTION
                      when others then
                       
                        p_status  := 'E';
                        p_message := 'Error in program_id ' || p_sequence ||
                                     ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ';
                        x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ' ||
                                     sqlerrm;
                      
                        IF (v_debug_status = 'YES') THEN
                          v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called';
                        
                        END IF;
                        EXIT;
                      
                    END;
                  
                    
                  
                  else
                  
                   
                    p_status  := 'W';
                    p_message := 'Error in INSERTION for INBD-PRSD flow ' ||
                                 individual_data_stream.data_stream;
                    x_message := 'Error in INSERTION for INBD-PRSD flow ' ||
                                 individual_data_stream.data_stream;
                    IF (v_debug_status = 'YES') THEN
                      v_debug_message := 'Error in INSERTION for INBD-PRSD flow ' ||
                                         individual_data_stream.data_stream;
                    
                    END IF;
                  
                  end if;
                
                end loop;
              
              else
              
                x_message := 'No INBD TO PRSD Rule Exist for ' ||
                             p_activity_name;
                p_message := 'Completed IP flow Upto for' ||
                             p_activity_name;
              
                IF (v_debug_status = 'YES') THEN
                  v_debug_message := 'No INBD TO PRSD Rule Exist for ' ||
                                     p_activity_name;
                
                END IF;
              
                BEGIN
                
                  GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                               x_message,
                                                               v_debug_message,
                                                               'W',
                                                               p_sequence,
                                                               '0');
                
                EXCEPTION
                  when others then
                   
                    p_status  := 'E';
                    p_message := 'Error in program_id ' || p_sequence ||
                                 ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                    x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ' ||
                                 sqlerrm;
                    IF (v_debug_status = 'YES') THEN
                      v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                    
                    END IF;
                  
                END;
              
              end if;
            
         
               
            
              IF (p_status != 'E') THEN
              
   ----------------------------- Before PRSD_SPM  START   --------------------------------
          GE_PLN_REQUEST_SET(p_activity_name,'BEFORE_PS',x_status,x_message);
        p_status := NVL(x_status,'S');
  ----------------------------- Before PRSD_SPM  END   --------------------------------
 
              
                x_message := 'STUB execution is started for PRSD to SPM flow for  ' ||
                             p_activity_name || ' ';
                IF (v_debug_status = 'YES') THEN
                  v_debug_message := 'STUB execution is started for PRSD to SPM flow ';
                
                END IF;
              
                BEGIN
                  GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE('GE_PRSD_SPM_FLOW',
                                                           'GE_PRSD_SPM_STUB',
                                                           SYSDATE,
                                                           x_message,
                                                           v_debug_message,
                                                           'I',
                                                           p_activity_name,
                                                           NULL,
                                                           NULL,
                                                           NULL,
                                                           p_sequence);
                
                EXCEPTION
                  when others then
                   
                    p_status  := 'E';
                    p_message := 'Error in program_id ' || p_sequence ||
                                 ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ';
                    x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ' ||
                                 sqlerrm;
                  
                    IF (v_debug_status = 'YES') THEN
                      v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ';
                    
                    END IF;
                  
                END;
                IF (p_status != 'E') THEN
                  
                  BEGIN
                  PDS.GE_PRSD_SPM_STUB.GE_PRSD_SPM_FLOW(p_activity_name,
                                                      p_status,
                                                      p_message);
                    --  p_message := 'Completed  Upto GE_PRSD_SPM_STUB STUB calling';
                    x_message := ' and Ended at ' || p_activity_name ||
                                 ' with ' || p_message;
                    -- x_message := 'and End with status ' || x_error ;
                    --  p_message := 'Completed GE_PRSD_SPM_STUB STUB ';
                  
                    IF (v_debug_status = 'YES') THEN
                      v_debug_message := 'Completed GE_PRSD_SPM_STUB STUB  with status ' ||
                                         x_error;
                    
                    END IF;
                  
                  EXCEPTION
                    when others then
                     
                      p_status  := 'E';
                      p_message := 'Error in program_id ' || p_sequence ||
                                   ' GE_PRSD_SPM_STUB.GE_INBD_PRSD_FLOWs can not be called ' || SQLERRM;
                      x_message := 'GE_PRSD_SPM_STUB.GE_INBD_PRSD_FLOWs can not be called ' ||
                                   sqlerrm;
                    
                      IF (v_debug_status = 'YES') THEN
                        v_debug_message := 'GE_PRSD_SPM_STUB.GE_INBD_PRSD_FLOWs can not be called '||SUBSTR(SQLERRM, 1, 200);
                      
                      END IF;
                    
                  END;
                
                  IF (p_status != 'E') THEN
                  
                    BEGIN
                    
                      GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                   x_message,
                                                                   v_debug_message,
                                                                   'W',
                                                                   p_sequence,
                                                                   '0');
                    
                    EXCEPTION
                      when others then
                       
                        p_status  := 'E';
                        p_message := 'Error in program_id ' || p_sequence ||
                                     ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                        x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ' ||
                                     sqlerrm;
                      
                        IF (v_debug_status = 'YES') THEN
                          v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                        
                        END IF;
                      
                    END;
                  
                    IF (p_status != 'E') THEN
                    
                      select count(*)
                        into num_of_records
                        from GE_SPM_RULE_HEADERS_ALL
                       where ACTIVITY_NAME = p_activity_name
                         and SPM_ENABLE_FLAG = 'Y';
                    
                      if (num_of_records > 0) then
                      
                       
                      
                        for individual_spm_data_stream in total_spm_enable_stream(p_activity_name) loop
                        
                          x_flow          := 'PS';
                          x_activity_type := 'INSERT';
                        
                          x_message := individual_spm_data_stream.data_stream ||
                                       ' for  ' || x_activity_type ||
                                       ' activity  in ' || x_flow ||
                                       ' flow Started at ';
                        
                          IF (v_debug_status = 'YES') THEN
                            v_debug_message := individual_spm_data_stream.data_stream ||
                                               ' for  ' || x_activity_type ||
                                               ' activity  in ' || x_flow ||
                                               ' flow Started ';
                          
                          END IF;
                          BEGIN
                          
                            GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE('execute_transformation',
                                                                     'GE_PLN_TRANSFORMATION',
                                                                     SYSDATE,
                                                                     x_message,
                                                                     v_debug_message,
                                                                     'I',
                                                                     p_activity_name,
                                                                     individual_spm_data_stream.data_stream,
                                                                     x_activity_type,
                                                                     x_flow,
                                                                     p_sequence);
                          EXCEPTION
                            when others then
                            
                              p_status  := 'E';
                              p_message := 'Error in program_id ' ||
                                           p_sequence ||
                                           ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ';
                              x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ' ||
                                           sqlerrm;
                            
                              IF (v_debug_status = 'YES') THEN
                                v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ' ||
                                                   sqlerrm;
                              END IF;
                            
                              EXIT;
                            
                          END;
                        
                          BEGIN
                            GE_PLN_TRANSFORMATION.execute_transformation(p_activity_name => p_activity_name,
                                                                         p_data_stream   => individual_spm_data_stream.data_stream,
                                                                         p_activity_type => x_activity_type,
                                                                         p_error         => x_error,
                                                                         p_flow          => x_flow,
                                                                         p_number        => x_number,
                                                                         p_rows          => p_rows);
                          
                            if (x_error = 'SUCCESS') THEN
                              x_message := 'Status : ' || x_error ||',' ||
                                           ' Number of Rows Processed ' ||
                                           x_number;
                              p_message := 'Completed PS flow Upto ' ||
                                           individual_spm_data_stream.data_stream ||
                                           ' for' || p_activity_name;
                              p_status  := 'W';
                            
                              IF (v_debug_status = 'YES') THEN
                                v_debug_message := 'Completed PS INSERT flow.. End with status ' ||
                                                   x_error ||
                                                   ' Number of Rows Processed ' ||
                                                   x_number;
                              END IF;
                            ELSIF (x_error = 'WARNING') THEN
                              x_message := 'Status : ' ||
                                           x_error || ' No Querie Exist' /*||
                                                                     ' Number of Rows Processed ' ||
                                                                     x_number*/
                               ;
                              p_message := 'Completed PS flow Upto ' ||
                                           individual_spm_data_stream.data_stream ||
                                           ' for' || p_activity_name;
                              p_status  := 'W'; 
                            
                              IF (v_debug_status = 'YES') THEN
                                v_debug_message := 'Completed PS INSERT flow.. Status ' ||
                                                   x_error /*||
                                                                                         ' Number of Rows Processed ' ||
                                                                                         x_number*/
                                                   || ' No Querie Exist';
                              END IF;
                            else
                              x_message := 'Status : ' ||
                                           x_error;
                              p_message := 'Error in program_id ' ||
                                           p_sequence;
                              -- p_status  := 'W';
                              p_status := 'E'; 
                              IF (v_debug_status = 'YES') THEN
                                v_debug_message := 'Error in PS flow';
                              END IF;
                              EXIT; 
                            
                            end if;
                          
                          exception
                            when others then
                              
                              p_status  := 'E';
                              p_message := 'Error in program_id ' ||
                                           p_sequence;
                              x_message := 'End with status ERROR - ' ||
                                           sqlerrm;
                              IF (v_debug_status = 'YES') THEN
                                v_debug_message := 'execute_transformation can not be called';
                              END IF;
                              EXIT;
                          end;
                          BEGIN
                          
                            GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                         x_message,
                                                                         v_debug_message,
                                                                         p_status,
                                                                         p_sequence,
                                                                         p_rows);
                          
                          EXCEPTION
                            when others then
                             
                              p_status  := 'E';
                              p_message := 'Error in program_id ' ||
                                           p_sequence ||
                                           ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                              x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ' ||
                                           sqlerrm;
                            
                              IF (v_debug_status = 'YES') THEN
                                v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called';
                              END IF;
                              EXIT;
                            
                          END;
                        
                          if (x_error = 'SUCCESS') THEN
                          
                            /* DBMS_OUTPUT.PUT_LINE('PRSD-SPM DATA PROCESSED for activity_name ' ||
                            p_activity_name ||
                            ' and data stream ' ||
                            individual_spm_data_stream.data_stream ||
                            ' for PS flow');*/
                          
                            -- PRSD table flag update ----
                            /* x_flow          := 'PS';*/
                            x_activity_type := 'UPDATE';
                          
                            x_message := individual_spm_data_stream.data_stream ||
                                         ' for  ' || x_activity_type ||
                                         ' activity  in ' || x_flow ||
                                         ' flow Started at ';
                          
                            IF (v_debug_status = 'YES') THEN
                              v_debug_message := individual_spm_data_stream.data_stream ||
                                                 ' for  ' ||
                                                 x_activity_type ||
                                                 ' activity  in ' || x_flow ||
                                                 ' flow Started ';
                            END IF;
                          
                            BEGIN
                              GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE('execute_transformation',
                                                                       'GE_PLN_TRANSFORMATION',
                                                                       SYSDATE,
                                                                       x_message,
                                                                       v_debug_message,
                                                                       'I',
                                                                       p_activity_name,
                                                                       individual_spm_data_stream.data_stream,
                                                                       x_activity_type,
                                                                       x_flow,
                                                                       p_sequence);
                            
                            EXCEPTION
                              when others then
                                
                                p_status  := 'E';
                                p_message := 'Error in program_id ' ||
                                             p_sequence ||
                                             ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ';
                                x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ' ||
                                             sqlerrm;
                              
                                IF (v_debug_status = 'YES') THEN
                                  v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE can not be called ';
                                END IF;
                                EXIT;
                              
                            END;
                            BEGIN
                              GE_PLN_TRANSFORMATION.execute_transformation(p_activity_name => p_activity_name,
                                                                           p_data_stream   => individual_spm_data_stream.data_stream,
                                                                           p_activity_type => x_activity_type,
                                                                           p_error         => x_error,
                                                                           p_flow          => x_flow,
                                                                           p_number        => x_number,
                                                                           p_rows          => p_rows);
                            
                              if (x_error = 'SUCCESS') THEN
                                x_message := 'Status : ' || x_error ||',' ||
                                             ' Number of Rows Processed ' ||
                                             x_number;
                                p_message := 'Completed IP flow Upto ' ||
                                             individual_spm_data_stream.data_stream ||
                                             ' for' || p_activity_name;
                                p_status  := 'W'; 
                                IF (v_debug_status = 'YES') THEN
                                  v_debug_message := 'Completed PS Update flow with status ' ||
                                                     x_error ||
                                                     ' Number of Rows Processed ' ||
                                                     x_number;
                                END IF;
                              ELSIF (x_error = 'WARNING') THEN
                                x_message := 'Status : ' || x_error ||',' ||
                                             ' No Querie Exist' /*||
                                                                       ' Number of Rows Processed ' ||
                                                                       x_number*/
                                 ;
                                p_message := 'Completed PS flow Upto ' ||
                                             individual_spm_data_stream.data_stream ||
                                             ' for' || p_activity_name;
                                p_status  := 'W'; 
                              
                                IF (v_debug_status = 'YES') THEN
                                  v_debug_message := 'Completed PS INSERT flow.. End with status ' ||
                                                     x_error /*||
                                                                                           ' Number of Rows Processed ' ||
                                                                                           x_number*/
                                                     || ' No Querie Exist';
                                END IF;
                              else
                                x_message := 'Status :' ||
                                             x_error;
                                -- p_status  := 'W';
                                IF (v_debug_status = 'YES') THEN
                                  v_debug_message := 'Error in PS flow ';
                                END IF;
                                p_status := 'E'; 
                                EXIT; 
                                p_message := 'Error in program_id ' ||
                                             p_sequence;
                              end if;
                            
                            exception
                              when others then
                               
                                p_status  := 'E';
                                p_message := 'Error in program_id ' ||
                                             p_sequence;
                                x_message := 'End with status ERROR - ' ||
                                             sqlerrm;
                              
                                IF (v_debug_status = 'YES') THEN
                                  v_debug_message := 'Error with status ERROR - ' ||
                                                     sqlerrm;
                                END IF;
                                EXIT;
                              
                            end;
                            GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                         x_message,
                                                                         v_debug_message,
                                                                         p_status,
                                                                         p_sequence,
                                                                         p_rows);
                          
                            /* if (x_error = 'SUCCESS') THEN
                            
                             \* DBMS_OUTPUT.PUT_LINE('UPDATED the INBD table for activity_name ' ||
                                                   p_activity_name ||
                                                   ' and data stream ' ||
                                                   individual_spm_data_stream.data_stream ||
                                                   ' for PS Flow');*\
                              \*if (x_error = 'SUCCESS') THEN*\
                            --  DBMS_OUTPUT.PUT_LINE('.....DONE......');
                              \*else
                                          DBMS_OUTPUT.PUT_LINE('.....Fail......');
                                        end if;
                              *\
                            
                            else
                            
                              \*DBMS_OUTPUT.PUT_LINE('Error in UPDATE for PRSD-SPM flow ' ||
                                                   individual_spm_data_stream.data_stream);*\
                                                   
                                 p_status  := 'W';
                                p_message := 'Error in UPDATE for PRSD-SPM flow ' ||
                                                   individual_spm_data_stream.data_stream;
                                 x_message := 'Error in UPDATE for PRSD-SPM flow ' ||
                                                   individual_spm_data_stream.data_stream;
                                                   
                            
                            end if;*/
                          else
                          
                           
                            p_status  := 'W';
                            p_message := 'Error in INSERT for PRSD-SPM flow ' ||
                                         individual_spm_data_stream.data_stream;
                            x_message := 'Error in INSERT for PRSD-SPM flow ' ||
                                         individual_spm_data_stream.data_stream;
                            IF (v_debug_status = 'YES') THEN
                              v_debug_message := 'Error in INSERT for PRSD-SPM flow ' ||
                                                 individual_spm_data_stream.data_stream;
                            END IF;
                          
                          end if;
                        
                        end loop;
                      
                        
                      else
                        
                        x_message := 'No PRSD TO SPM Rule Exist for ' ||
                                     p_activity_name;
                        p_message := 'Completed PS flow Upto for' ||
                                     p_activity_name;
                      
                        BEGIN
                        
                          GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                       x_message,
                                                                       v_debug_message,
                                                                       'W',
                                                                       p_sequence,
                                                                       '0');
                        EXCEPTION
                          when others then
                           
                            p_status  := 'E';
                            p_message := 'Error in program_id ' ||
                                         p_sequence ||
                                         ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                            x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ' ||
                                         sqlerrm;
                          
                            IF (v_debug_status = 'YES') THEN
                              v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                            END IF;
                          
                        END;
                      
                      end if;
                    
                    
         
                      IF (p_status != 'E') THEN
                      
 ----------------------------- AFTER PS  START   --------------------------------
  
  GE_PLN_REQUEST_SET(p_activity_name,'AFTER_PS',x_status,x_message);
  p_status := NVL(x_status,'S');
  
  ----------------------------- AFTER PS  END   --------------------------------
 
                      
                        x_message := 'STUB execution is started after data flow in SPM table for  ' ||
                                     p_activity_name || ' ';
                      
                        IF (v_debug_status = 'YES') THEN
                          v_debug_message := 'GE_SPM_STUB_LOGIC STUB execution is started ';
                        END IF;
                        BEGIN
                          GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE('GE_PRSD_SPM_FLOW',
                                                                   'GE_SPM_STUB_LOGIC',
                                                                   SYSDATE,
                                                                   x_message,
                                                                   v_debug_message,
                                                                   'I',
                                                                   p_activity_name,
                                                                   NULL,
                                                                   NULL,
                                                                   NULL,
                                                                   p_sequence);
                        
                        EXCEPTION
                          when others then
                          
                            p_status  := 'E';
                            p_message := 'Error in program_id ' ||
                                         p_sequence ||
                                         ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                            x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ' ||
                                         sqlerrm;
                          
                            IF (v_debug_status = 'YES') THEN
                              v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                            END IF;
                          
                        END;
                      
                        IF (p_status != 'E') THEN
                        
                        
                        
                          BEGIN
                            PDS.GE_SPM_STUB_LOGIC.GE_SPM_FLOW(p_activity_name,
                                                              p_status,
                                                              p_message);
                          
                            x_message := 'Ended at ' ||
                                         p_activity_name || ' with status ' ||
                                         p_message;
                            /*p_message := 'Completed  Upto final STUB calling for' ||
                            p_activity_name;*/
                          
                            IF (v_debug_status = 'YES') THEN
                              v_debug_message := ' Completed stub at ' ||
                                                 p_activity_name ||
                                                 ' with status ' ||
                                                 p_message;
                            END IF;
                          
                          EXCEPTION
                            when others then
                             
                              p_status  := 'E';
                              p_message := 'Error in program_id ' ||
                                           p_sequence ||
                                           ' GE_SPM_STUB_LOGIC.GE_SPM_FLOW can not be called ';
                              x_message := 'GE_SPM_STUB_LOGIC.GE_SPM_FLOW can not be called ' ||
                                           sqlerrm;
                            
                              IF (v_debug_status = 'YES') THEN
                                v_debug_message := ' GE_SPM_STUB_LOGIC.GE_SPM_FLOW can not be called ';
                              END IF;
                            
                          END;
                         
                          IF (p_status != 'E') THEN
                          
                            BEGIN
                              GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                           x_message,
                                                                           v_debug_message,
                                                                           'W',
                                                                           p_sequence,
                                                                           '0');
                            
                            EXCEPTION
                              when others then
                                
                                p_status  := 'E';
                                p_message := 'Error in program_id ' ||
                                             p_sequence ||
                                             ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                                x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ' ||
                                             sqlerrm;
                                IF (v_debug_status = 'YES') THEN
                                  v_debug_message := ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                                END IF;
                              
                            END;
                            IF (p_status != 'E') THEN
                              p_status  := 'S';
                              p_message := 'Success for program_id ' ||
                                           p_sequence;
                              x_message := p_sequence || ' COMPLETED ';
                              IF (v_debug_status = 'YES') THEN
                                v_debug_message := p_sequence ||
                                                   ' COMPLETED ';
                              END IF;
                            
                              BEGIN
                              
                                GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                             x_message,
                                                                             v_debug_message,
                                                                             'C',
                                                                             p_sequence,
                                                                             '0');
                              
                              EXCEPTION
                                when others then
                                 
                                  p_status  := 'E';
                                  p_message := 'Error in program_id ' ||
                                               p_sequence ||
                                               ' GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                                  x_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ' ||
                                               sqlerrm;
                                  IF (v_debug_status = 'YES') THEN
                                    v_debug_message := 'GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END can not be called ';
                                  END IF;
                                
                              END;
                              
                              BEGIN
                              
                              select TRANSLATED_VALUE
                              INTO V_MW_CALL
                                from GE_PLN_TRANSLATION_LOOKUP
                               where lookup_type = 'MW_LAST_STUB_CALL'
                                 and value = p_activity_name;
                                 
                                 EXCEPTION
                                  when others then
                                    
                                    V_MW_CALL := 'N';
                                  
                                END;
                              
                                IF (V_MW_CALL = 'N' ) THEN
                            
                             
                            
                              IF (p_status != 'E') THEN
                                x_message := 'Last STUB execution is started   ' ||
                                             p_activity_name || ' ';
                              
                                IF (v_debug_status = 'YES') THEN
                                  v_debug_message := 'Last STUB execution is started  ';
                                END IF;
                                
----------------------------- BEFORE PS  START   --------------------------------
  
  GE_PLN_REQUEST_SET(p_activity_name,'BEFORE_ARCHIVE',x_status,x_message);
  p_status := NVL(x_status,'S');
  
  ----------------------------- AFTER PS  END   --------------------------------
                              
                                GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE('GE_PRSD_SPM_FLOW',
                                                                         'GE_SPM_STUB',
                                                                         SYSDATE,
                                                                         x_message,
                                                                         v_debug_message,
                                                                         'R', --'I', To remove dependencies
                                                                         p_activity_name,
                                                                         NULL,
                                                                         NULL,
                                                                         NULL,
                                                                         p_sequence);
                                BEGIN
                                
                                  PDS.GE_SPM_STUB.GE_SPM_FLOW(p_activity_name,
                                                              p_status,
                                                              p_message);
                                
                                  x_message := ' Ended at ' ||
                                               p_activity_name ||
                                               ' with status ' || p_message;
                                  /* p_message := 'Completed  Upto final STUB calling for' ||
                                  p_activity_name;*/
                                
                                  IF (v_debug_status = 'YES') THEN
                                    v_debug_message := 'Completed  Upto final STUB calling  ';
                                  END IF;
                                EXCEPTION
                                  when others then
                                    
                                    p_status  := 'E';
                                    p_message := 'Error in program_id ' ||
                                                 p_sequence ||
                                                 ' GE_SPM_STUB.GE_SPM_FLOW can not be called ' ||
                                                 sqlerrm;
                                    x_message := 'GE_SPM_STUB.GE_SPM_FLOW can not be called ' ||
                                                 sqlerrm;
                                  
                                    IF (v_debug_status = 'YES') THEN
                                      v_debug_message := 'GE_SPM_STUB.GE_SPM_FLOW can not be called ' ||
                                                 sqlerrm;
                                    END IF;
                                  
                                END;
                                IF (p_status != 'E') THEN
                                
                                
                                
                                 
                                  GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                               x_message,
                                                                               v_debug_message,
                                                                               'C',
                                                                               p_sequence,
                                                                               '0');
                                                                               
                                  BEGIN
                                    UPDATE GEMS_IFACE_SPM_TABLE
                                       set MESSAGE = (select MESSAGE from  GEMS_IFACE_SPM_TABLE  WHERE process_id = p_sequence)
                                                     || CHR(13) ||
                                                     ' Completed Successfully at ' ||
                                                     to_char(SYSDATE,
                                                             'DD-MON-YYYY HH:MI:ss') --  Due to defect ...Issue#096
                                    
                                     WHERE process_id = p_sequence;
                                     COMMIT;
                                  
                                  EXCEPTION
                                    when others then
                                      p_status := 'E';
                                  END;
                                
                                ELSE
                                  x_message := p_message;
                                
                                  GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                               x_message,
                                                                               v_debug_message,
                                                                               'E',
                                                                               p_sequence,
                                                                               '0');
                                
                                END IF;
                              
                                
                              
                              ELSE
                                GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                             x_message,
                                                                             v_debug_message,
                                                                             'E',
                                                                             p_sequence,
                                                                             '0');
                              
                              END IF;
                                
                                  END IF;            
                            ELSE
                            
                              GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                           x_message,
                                                                           v_debug_message,
                                                                           'E',
                                                                           p_sequence,
                                                                           '0');
                            
                            END IF;
                          
                          ELSE
                          
                            x_message := p_message;
                            GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                         x_message,
                                                                         v_debug_message,
                                                                         'E',
                                                                         p_sequence,
                                                                         '0');
                          
                          END IF;
                        
                        ELSE
                          GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                       x_message,
                                                                       v_debug_message,
                                                                       'E',
                                                                       p_sequence,
                                                                       '0');
                        
                        END IF;
                      
                      ELSE
                        GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                     x_message,
                                                                     v_debug_message,
                                                                     'E',
                                                                     p_sequence,
                                                                     '0');
                      
                      END IF;
                    ELSE
                      GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                   x_message,
                                                                   v_debug_message,
                                                                   'E',
                                                                   p_sequence,
                                                                   '0');
                    
                    END IF;
                  ELSE
                    
                    x_message := p_message;
                    GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                                 x_message,
                                                                 v_debug_message,
                                                                 'E',
                                                                 p_sequence,
                                                                 '0');
                  
                  END IF;
                
                ELSE
                  GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                               x_message,
                                                               v_debug_message,
                                                               'E',
                                                               p_sequence,
                                                               '0');
                END IF;
              
              ELSE
                GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                             x_message,
                                                             v_debug_message,
                                                             'E',
                                                             p_sequence,
                                                             '0');
              
              END IF;
            ELSE
              GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                           x_message,
                                                           v_debug_message,
                                                           'E',
                                                           p_sequence,
                                                           '0');
            
            END IF;
          ELSE
            x_message := p_message;
            GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                         x_message,
                                                         v_debug_message,
                                                         'E',
                                                         p_sequence,
                                                         '0');
          
          END IF;
        ELSE
          GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                       x_message,
                                                       v_debug_message,
                                                       'E',
                                                       p_sequence,
                                                       '0');
        
        END IF;
      ELSE
        GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                     x_message,
                                                     v_debug_message,
                                                     'E',
                                                     p_sequence,
                                                     '0');
      
      END IF;
    ELSE
    
      IF (p_activity_name IS NULL) THEN
      
        p_status  := 'E';
        p_message := 'Activity Type can not be NULL ';
        x_message := 'Activity Type can not be NULL ';
      ELSE
        p_status  := 'E';
        p_message := 'Procedure cannot be called for more than one time for Activity Type: ' ||
                     p_activity_name;
        x_message := 'Procedure cannot be called for more than one time for Activity Type: ' ||
                     p_activity_name;
      
      END IF;
    
      IF (v_debug_status = 'YES') THEN
        v_debug_message := 'Procedure cannot be called for more than one time for Activity Type: ' ||
                           p_activity_name;
      END IF;
      GE_IFACE_SPM_DETAILS.GE_IFACE_SPM_UPDATE_END(SYSDATE,
                                                   x_message,
                                                   v_debug_message,
                                                   'E',
                                                   p_sequence,
                                                   '0');
    
    END IF;
  END GE_PLN_TRANSFORMATION_FLOW;


PROCEDURE GE_PLN_REQUEST_SET(p_activity_name IN VARCHAR2,
                                   P_ACTIVITY_TYPE IN VARCHAR2,
                                   x_status        OUT VARCHAR2,
                                   x_message       OUT VARCHAR2) IS
                                       
  C_MAPVALUE               VARCHAR2(100) := 'DAILY';
  C_EXECUTE                VARCHAR2(100) := 'N';
  C_NUMBER_OF_RUN          NUMBER  := 0;
  V_DAY                    VARCHAR2(100) := 'FRI';
  V_SQL                    CLOB := 'NA';
 -- X_RETURN                 VARCHAR2 (1000);  DB UPGRADE # GECHG1668910
  V_WAIT_TIME              NUMBER := 0;
  V_STATUS                 NUMBER := 0;  
  PARENT_activity_name     VARCHAR2(1000);
  

  
CURSOR C_REQUEST_SET(PARENT_activity_name VARCHAR2, P_ACTIVITY_TYPE VARCHAR2) IS
  SELECT * FROM PDS.GE_PLN_FUNCTIONAL_MAPPING_TBL 
  WHERE MAPPING_TYPE = 'REQUESTSET'
  AND INTERNAL_USE = PARENT_activity_name
  AND ACTIVITY_TYPE = P_ACTIVITY_TYPE
  AND CHAR_VALUE1 = 'Y'
  ORDER BY MAP_VALUE1,NUMERIC_VALUE1;
  
  
  BEGIN
  select TO_CHAR(sysdate,'DY') INTO V_DAY from dual;
  PARENT_activity_name := p_activity_name;
  
  FOR C_REQUEST_SET_REC IN C_REQUEST_SET(PARENT_activity_name,P_ACTIVITY_TYPE ) LOOP
      C_MAPVALUE :=  NVL(C_REQUEST_SET_REC.MAP_VALUE1,'DAILY');
      C_EXECUTE := 'N';    
          
      IF (C_MAPVALUE = 'DAILY' AND C_REQUEST_SET_REC.NUMERIC_VALUE2 IS NULL) THEN
    
      C_EXECUTE := 'Y';
       
  ELSIF (C_MAPVALUE = 'DAILY' AND C_REQUEST_SET_REC.NUMERIC_VALUE2 =1 ) THEN 
        
        BEGIN
        SELECT COUNT(*) 
        INTO C_NUMBER_OF_RUN
        FROM  PDS.GEMS_IFACE_SPM_TABLE
        WHERE TRUNC(CREATION_DATE) >= TRUNC(SYSDATE)
        AND ACTIVITY_NAME = PARENT_activity_name
        AND STATUS_FLAG = 'C'  
        ;
        EXCEPTION WHEN OTHERS THEN 
        C_NUMBER_OF_RUN := 0;
        END;
        
        IF(C_NUMBER_OF_RUN >=1 ) THEN
             C_EXECUTE := 'N';
        ELSE
            C_EXECUTE := 'Y';              
        END IF;
        
  ELSIF (C_MAPVALUE = 'WEEKLY' AND NVL(C_REQUEST_SET_REC.MAP_VALUE2,'FRI') = V_DAY) THEN
        C_EXECUTE := 'Y';  
         
  ELSIF (C_MAPVALUE = 'MONTHLY') THEN

          C_NUMBER_OF_RUN := 0 ;
          BEGIN
        SELECT COUNT(*) 
        INTO C_NUMBER_OF_RUN
        FROM  PDS.GEMS_IFACE_SPM_TABLE A
        WHERE TO_CHAR(A.CREATION_DATE,'MON')|| TO_CHAR(A.CREATION_DATE,'YYYY')  = TO_CHAR(sysdate,'MON')|| TO_CHAR(sysdate,'YYYY')
        AND A.ACTIVITY_NAME = PARENT_activity_name
        AND STATUS_FLAG = 'C' 
        ;
        EXCEPTION WHEN OTHERS THEN 
        C_NUMBER_OF_RUN := 0;
        END;
         
        IF(C_NUMBER_OF_RUN >=1 ) THEN
             C_EXECUTE := 'N';
        ELSE
            C_EXECUTE := 'Y';             
        END IF;
         
          END IF;
   IF (C_EXECUTE = 'Y' ) THEN
   
  
BEGIN
   V_SQL := 'declare x_status varchar2(1000); x_message varchar2(1000); begin GE_PLN_TRANSFORMATION_CALL.GE_PLN_TRANSFORMATION_FLOW(''' ||
            C_REQUEST_SET_REC.ACTIVITY_NAME ||--';'||p_sequence ||
            ''',x_status,x_message); end;';
 
EXCEPTION
   when others then
   
     x_status  := 'E';
     x_message := 'ERROR';

 END;
 
 BEGIN
     DBMS_SCHEDULER.create_job (
    job_name        => 'SPM_CHILD_JOB'||C_REQUEST_SET_REC.ACTIVITY_NAME ,
    job_type        => 'PLSQL_BLOCK',
    job_action      => V_SQL,
    start_date      => SYSTIMESTAMP,
    repeat_interval => 'freq=hourly; byminute=0',
    end_date        => NULL,
    enabled         => TRUE,
    comments        => 'SPM_CHILD_JOB');
   
   DBMS_SCHEDULER.run_job (job_name            => 'SPM_CHILD_JOB'||C_REQUEST_SET_REC.ACTIVITY_NAME ,
                          use_current_session => TRUE);

END;
/* DBMS_JOB.SUBMIT(x_return, V_SQL); -- Q419 -- 20191116
   DBMS_JOB.RUN(X_RETURN, FALSE);*/ -- DB UPGRADE # GECHG1668910
   
 BEGIN  
   SELECT MAX(PROCESS_ID)
     INTO P_SEQUENCE
     FROM PDS.GEMS_IFACE_SPM_TABLE
    WHERE TRUNC(CREATION_DATE) >= TRUNC(SYSDATE)
      and activity_name = C_REQUEST_SET_REC.INTERNAL_USE
      AND STATUS_FLAG not in ('C','E');

 EXCEPTION
   when others then
   
     x_status  := 'E';
     x_message := 'ERROR';

 END;
      
     ------------- Wait Logic for Master_Trnsformation_logic to be completed ------------
    /* dbms_output.put_line(SYSTIMESTAMP);*/
   --DBMS_LOCK.sleep (10);
   /*  dbms_output.put_line(SYSTIMESTAMP);*/
   
   
  v_wait_time := 0;
  v_status := 0;     
         
         WHILE v_wait_time = 0
          LOOP
          BEGIN
         Select count(*)
        INTO v_status
        FROM GEMS_IFACE_SPM_TABLE 
        WHERE ACTIVITY_NAME = C_REQUEST_SET_REC.ACTIVITY_NAME
        AND creation_date >= sysdate - 720 / 1440
        AND PROCESS_ID = (SELECT PROCESS_ID FROM (SELECT PROCESS_ID FROM GEMS_IFACE_SPM_TABLE
                           WHERE ACTIVITY_NAME = C_REQUEST_SET_REC.ACTIVITY_NAME
                           AND creation_date >= sysdate - 720 / 1440
                           AND nvl(STATUS_FLAG,'I') not in ('E','C')
                           ORDER BY creation_date desc ) PROC_TAB WHERE ROWNUM =1  
                           )
        AND nvl(STATUS_FLAG,'I') not in ('E','C');
       exception
      when others then
       v_status := 0;
    end;
    
    if (v_status = 0) then
       v_wait_time := 1;
    else
       BEGIN
          --  DBMS_LOCK.sleep (30); -- DB UPGRADE # GECHG1668910
           DBMS_SESSION.sleep (30); 
       exception when others then
           p_status  := 'E';
       end;
    end if;
 
    END LOOP;
    
    DBMS_SCHEDULER.drop_job (job_name => 'SPM_CHILD_JOB'||C_REQUEST_SET_REC.ACTIVITY_NAME ); -- DB UPGRADE # GECHG1668910
  
  END IF;
  
  END LOOP;
   
  END GE_PLN_REQUEST_SET;


END GE_PLN_TRANSFORMATION_CALL;
