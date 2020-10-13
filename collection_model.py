# Copyright 2020 Vitalant and W. Alton Russell
# Authors: Eduard Grebe, W. Alton Russell, Brian Custer
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# The software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this software. If not, see <http://www.gnu.org/licenses/>.


# The microsimulation code has to be imported as module inside the 
# "if __name__ == '__main__':" guard to ensure that the multiprocessing module
# does not cause an undesired recursion
if __name__ == '__main__':
    
    import microsimulation as ms
    
    def count_cores():
        return ms.count_cores()

    def run_simulation(parameters, seed=None, progress_callback=None, iden = 0):
        def progress(val):
            progress_callback(val)
        sim = ms.run_simulation(parameters, seed=seed, progress_callback=progress, iden=iden)
        return sim

    def multi_threaded_run(parameter_list,
                       simulations=4,
                       processes=count_cores(),
                       root_seed = None, # Not used currently
                       simset_name = "mutiple_simulations",
                       output_report = False,
                       output_agents = False,
                       output_parameters = False,
                       report_dir = "./",
                       return_results = True,
                       progress_callback=None):
        if return_results:
            sim = ms.multi_threaded_run(parameter_list,
                                        simulations=simulations,
                                        processes=processes,
                                        root_seed=root_seed,
                                        simset_name=simset_name,
                                        output_report=output_report,
                                        output_agents=output_agents,
                                        output_parameters=output_parameters,
                                        report_dir=report_dir,
                                        return_results=return_results,
                                        progress_callback=progress_callback)
            return sim
        else:
            ms.multi_threaded_run(parameter_list,
                                  simulations=simulations,
                                  processes=processes,
                                  root_seed=root_seed,
                                  simset_name=simset_name,
                                  output_report=output_report,
                                  output_agents=output_agents,
                                  output_parameters=output_parameters,
                                  report_dir=report_dir,
                                  return_results=return_results,
                                  progress_callback=progress_callback)
                                  
